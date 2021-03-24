package fpinscala.applicative

import fpinscala.monad.Functor
import fpinscala.monad.Monad

trait Applicative[F[_]] extends Functor[F] { self =>
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    apply_map2(fab)(fa)
  def map_apply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)
  def map2_apply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)
  def apply_map2[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  // 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // 12.8
  def product[G[_]](G: Applicative[G]) =
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A) =
        (self.unit(a), G.unit(a))
      def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
    }

  // 12.9
  def compose[G[_]](G: Applicative[G]) =
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A) =
        self.unit(G.unit(a))
      def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C) =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }
}

object Applicative {

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A) =
      Stream.continually(a)
    def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C) =
      fa zip fb map f.tupled
  }

  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A): Either[E, A] =
      Right(a)
    def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma match {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
      }
  }
}

// 12.6
sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A) =
        Success(a)
      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ) = (fa, fb) match {
        case (Success(a), Success(b))           => Success(f(a, b))
        case (Success(_), Failure(h, t))        => Failure(h, t)
        case (Failure(h, t), Success(_))        => Failure(h, t)
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
      }

    }
}

// trait Monad[F[_]] extends Applicative[F] {
//   def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
//     join(map(fa)(f))
//   def join[A](ffa: F[F[A]]): F[A] =
//     flatMap(ffa)(identity)
//   def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
//     a => flatMap(f(a))(g)
//   override def map[A, B](fa: F[A])(f: A => B): F[B] =
//     flatMap(fa)(a => unit(f(a)))
//   def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
//     flatMap(fa)(a => map(fb)(b => f(a, b)))
// }
