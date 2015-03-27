package fpinscala
package monads

import parsing._
import testing._
import parallelism.Nonblocking._
import state._
import parallelism.Nonblocking.Par._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List.empty[B])) { (a, mbs) =>
      map2(f(a), mbs)(_ :: _)
    }
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](as: List[A])(f: A => M[Boolean]): M[List[A]] =
    as.foldRight(unit(List.empty[A])) { (a, mas) =>
      map2(f(a), mas) { (b, as) =>
        if (b) a :: as
        else as
      }
    }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = { a =>
    flatMap(f(a))(g)
  }

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = ma.flatMap(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
    override def unit[A](a: => A): P[A] = p.succeed(a)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)
    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
    override def unit[A](a: => A): List[A] = List(a)
  }

  type IntState[A] = State[Int, A]

  val intStateMonad = new Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State.unit(a)
    override def flatMap[A, B](ma: IntState[A])(f: (A) => IntState[B]): IntState[B] =
      ma flatMap f
  }

  object stringStateMonad extends Monad[({ type StringState[A] = State[String, A] })#StringState] {
    override def unit[A](a: => A): State[String, A] = State.unit(a)
    override def flatMap[A, B](ma: State[String, A])(f: A => State[String, B]): State[String, B] =
      ma flatMap f
  }

  def stateMonad[S] = new Monad[({ type f[A] = State[S, A] })#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = Reader.readerMonad
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    override def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader { r =>
      f(st.run(r)).run(r)
    }
  }
}

