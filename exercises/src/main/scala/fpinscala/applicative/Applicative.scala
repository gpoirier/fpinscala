package fpinscala
package applicative

import monads.Functor
import state._
import State._
import monoids._

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fabc = unit[A => (B => C)](f.curried)
    val fbc = apply(fabc)(fa)
    apply(fbc)(fb)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)(_.apply(_))
  }

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])
                   (f: (A, B, C) => D) : F[D] = {
    val fabcd = unit[A => (B => C => D)](f.curried)
    val fbcd = apply(fabcd)(fa)
    val fcd = apply(fbcd)(fb)
    apply(fcd)(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fabcde = unit[A => (B => C => D => E)](f.curried)
    val fbcde = apply(fabcde)(fa)
    val fcde = apply(fbcde)(fb)
    val fde = apply(fcde)(fc)
    apply(fde)(fd)
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }


  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val F = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = F.unit(a) -> G.unit(a)
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        F.map2(fa._1, fb._1)(f) -> G.map2(fa._2, fb._2)(f)
      }
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val F = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        F.map2(fa, fb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map.empty[K, V])) { case (acc, (key, fv)) =>
      map2(acc, fv) { (map, value) =>
        map + (key -> value)
      }
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  override def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma) { a => unit(f(a)) }
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](either: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      either.right flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N])
      : Monad[({type f[x] = F[N[x]]})#f] = {
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

      override def flatMap[A, B](fna: F[N[A]])(f: (A) => F[N[B]]): F[N[B]] = {
        F.flatMap(fna) { na =>
          val nested = T.sequence(N.map(na)(f))
          F.map(nested)(N.join)
        }
      }
    }
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty)
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A,B,C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(_), f @ Failure(_, _)) => f
        case (f @ Failure(_, _), Success(_)) => f
        case (Failure(head1, tail1), Failure(head2, tail2)) =>
          Failure(head1, (tail1 :+ head2) ++ tail2)
      }
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse) {
      (_, as) => as.head -> as.tail
    }._1
  }

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(fa, z) { (a, b) => () -> f(b, a) }._2
  }

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa) { a =>
      f(a) -> g(a)
    }(G product H)
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val F = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] = {
        F.traverse(fga)(G.traverse(_)(f))
      }
    }
  }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](as: List[A])(f: (A) => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      as.foldRight(G.unit(List.empty[B])) { (a, gbs) =>
        G.map2(f(a), gbs)(_ :: _)
      }
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
      fa.fold(G.unit(Option.empty[B])) { a =>
        G.map(f(a))(Some(_))
      }
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] = {
      val G = implicitly[Applicative[G]]
      val head = f(fa.head)
      val tail = listTraverse.traverse(fa.tail)(traverse(_)(f))
      G.map2(head, tail)(Tree(_, _))
    }
  }
}
