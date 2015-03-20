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

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

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

  lazy val wcMonoid: Monoid[WC] = sys.error("todo")

  def count(s: String): Int = sys.error("todo")

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

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

