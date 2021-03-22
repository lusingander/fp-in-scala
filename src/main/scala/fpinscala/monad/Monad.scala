package fpinscala.monad

import fpinscala.testing.Gen
import fpinscala.parallelism.Par
import fpinscala.laziness.Stream
import fpinscala.state.State

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, l) => map2(ma, l)(_ :: _))
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_ :: _))

  // 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // 11.6
  // def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = {
  //   val x: A => (A => F[Boolean]) => F[Option[A]] =
  //     a => g => map(g(a))(b => if (b) Some(a) else None)
  //   val y: List[F[Option[A]]] = la.map(a => x(a)(f))
  //   val z: F[List[Option[A]]] = sequence(y)
  //   map(z)(ol => ol.flatten)
  // }
  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] =
    la match {
      case Nil => unit(Nil)
      case head :: next =>
        flatMap(f(head))(b =>
          if (b) map2(unit(head), filterM(next)(f))(_ :: _)
          else filterM(next)(f)
        )
    }

  // 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // 11.8
  def flatMap_compose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  // 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  // 11.13
  def flatMap_join[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(a => f(a)))
  def compose_join[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  // 11.1
  val parMonad = new Monad[Par.Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A, B](ma: Par.Par[A])(f: A => Par.Par[B]) = Par.flatMap(ma)(f)
  }
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma.flatMap(f)
  }
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma.flatMap(f)
  }
  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A) = State(s => (a, s))
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]) = ma.flatMap(f)
  }
}

// 11.17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]) = ma.flatMap(f)
  }
}

// 11.20
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(ma.run(r)).run(r))
  }
}
