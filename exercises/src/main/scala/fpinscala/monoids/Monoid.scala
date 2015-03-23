package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.ParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import fpinscala.testing._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  implicit class MonoidOps[T: Monoid](val left: T) {
    def |+|(right: T): T = implicitly[Monoid[T]].op(left, right)
  }

  def apply[A](zeroValue: A)(f: (A, A) => A) = new Monoid[A] {
    def op(a1: A, a2: A): A = f(a1, a2)
    val zero: A = zeroValue
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = Monoid(0)(_ + _)

  val intMultiplication: Monoid[Int] = Monoid(1)(_ * _)

  val booleanOr: Monoid[Boolean] = Monoid(false)(_ || _)

  val booleanAnd: Monoid[Boolean] = Monoid(true)(_ && _)

  def optionMonoid[A]: Monoid[Option[A]] = Monoid(Option.empty[A])(_ orElse _)

  def endoMonoid[A]: Monoid[A => A] = Monoid(identity[A](_))(_ andThen _)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    implicit val _ = m

    Prop("has a zero value").forAll(gen) { a =>
      (a |+| m.zero) == a && (m.zero |+| a) == a
    } &&
      Prop("is associative").forAll(for {
        a <- gen
        b <- gen
        c <- gen
      } yield (a, b, c)) {
        case (a, b, c) =>
          ((a |+| b) |+| c) == (a |+| (b |+| c))
      }
  }

  def dual[A](m: Monoid[A]): Monoid[A] =
    Monoid(m.zero) { (a1, a2) => m.op(a2, a1) }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    foldMap(as, m)(identity)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (b, a) => m.op(b, f(a)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length <= 1) as.headOption.fold(m.zero)(f)
    else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    sealed trait State
    case object Empty extends State
    case object Unordered extends State
    case class Ordered(min: Int, max: Int) extends State

    val m: Monoid[State] = Monoid[State](Empty) {
      case (Empty, a) => a
      case (a, Empty) => a
      case (Ordered(min1, max1), Ordered(min2, max2)) if max1 <= min2 =>
        Ordered(min1, max2)
      case _ => Unordered
    }
    foldMapV(ints, m)(a => Ordered(a, a)) != Unordered
  }

  sealed trait WC {
    def count: Int = this match {
      case Stub(text) => WC.textRegex.findAllMatchIn(text).length
      case Part(lStub, count, rStub) => Stub(lStub).count + count + Stub(rStub).count
    }
  }
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  object WC {
    private val blankRegex = "\\W+".r
    private val textRegex = "\\w+".r

    def apply(text: String): WC = {
      val matches = blankRegex.findAllIn(text).matchData.toVector

      if (matches.length == 0)
        Stub(text)
      else
        Part(
          text.substring(0, matches.head.end),
          matches.length - 1,
          text.substring(matches.last.start))
    }

    val empty: WC = Stub("")
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    Monoid(Par.unit(m.zero)) { (a, b) => (a map2 b)(m.op) }

  def parFoldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    if (as.length <= 1) Par.unit(as.headOption.fold(m.zero)(f))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      par(m).op(
        Par.fork(parFoldMap(left, m)(f)),
        Par.fork(parFoldMap(right, m)(f)))
    }

  def parFoldMap2[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    Par.parMap(as)(f) flatMap { v =>
      foldMapV(v, par(m))(Par.unit)
    }
  }

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {

    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(chars1), Stub(chars2)) =>
        WC(chars1 + chars2)
      case (Stub(chars), right @ Part(lStub, _, _)) =>
        WC(chars + lStub) match {
          case Stub(newLStub) => right.copy(lStub = newLStub)
          case left => op(left, right.copy(lStub = ""))
        }
      case (left @ Part(_, _, rStub), Stub(chars)) =>
        WC(rStub + chars) match {
          case Stub(newRStub) => left.copy(rStub = newRStub)
          case right => op(left.copy(rStub = ""), right)
        }
      case (Part(lStub1, count1, rStub1), Part(lStub2, count2, rStub2)) =>
        Part(lStub1, count1 + WC(rStub1 + lStub2).count + count2, rStub2)
    }

    def zero = WC.empty
  }

  def count(s: String): Int = {

    def go(text: String): WC = {
      if (text.length < 10)
        Stub(text)
      else {
        val (left, right) = text.splitAt(text.length / 2)
        wcMonoid.op(go(left), go(right))
      }
    }

    go(s).count
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { (b, a) => mb.op(b, f(a)) }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(left, right) =>
        foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) =>
        foldRight(left)(foldRight(right)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.fold(z)(f(z, _))

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.fold(z)(f(_, z))
}

