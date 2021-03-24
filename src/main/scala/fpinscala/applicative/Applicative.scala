package fpinscala.applicative

import fpinscala.monad.Functor
import fpinscala.monoid.Monoid
import fpinscala.monoid.Foldable
import fpinscala.state.State

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

  // 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]())) { case ((k, fv), m) =>
      map2(fv, m)((v, mm) => mm.updated(k, v))
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
    override def flatMap[A, B](
        ma: Either[E, A]
    )(f: A => Either[E, B]): Either[E, B] =
      ma match {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
      }
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A) = M.zero
      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C) = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  // 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ???

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[
      ({ type f[x] = Applicative.Const[M, x] })#f,
      A,
      Nothing
    ](as)(f)(Applicative.monoidApplicative(mb))

  def foldRight[A, M](as: F[A])(z: M)(f: (A, M) => M): M = ???
  def foldLeft[A, M](as: F[A])(z: M)(f: (M, A) => M): M = ???
  def concatenate[A](as: F[A])(m: Monoid[A]): A = ???

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) =>
      (for {
        i <- StateUtil.get[Int]
        _ <- StateUtil.set(i + 1)
      } yield (a, i))
    ).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) =>
      (for {
        as <- StateUtil.get[List[A]]
        _ <- StateUtil.set(a :: as)
      } yield ())
    ).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      (for {
        s1 <- StateUtil.get[S]
        (b, s2) = f(a, s1)
        _ <- StateUtil.set(s2)
      } yield b)
    ).run(s)

  def zipWithIndex_mapAccum[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
  def toList_mapAccum[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  // 12.16
  def reverse[A](fa: F[A]): F[A] =
    ???

  // 12.17
  def foldLeft_[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  // 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit
      G: Applicative[G],
      H: Applicative[H]
  ): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(
      G.product(H)
    )

  // 12.19
  def compose[G[_]](implicit
      G: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] =
    new Traverse[({ type f[x] = F[G[x]] })#f] {
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(
          f: A => H[B]
      ): H[F[G[B]]] =
        self.traverse(fga)(G.traverse(_)(f))
    }
}

object Traverse {
  // 12.13
  val listTraverse = new Traverse[List] {
    def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit
        g: Applicative[G]
    ): G[List[B]] =
      fa.foldRight(g.unit(List[B]()))((a, b) => g.map2(f(a), b)(_ :: _))
  }
  val optionTraverse = new Traverse[Option] {
    def traverse[G[_], A, B](
        fa: Option[A]
    )(f: A => G[B])(implicit g: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => g.map(f(a))(Some(_))
        case None    => g.unit(None)
      }
  }
  case class Tree[+A](head: A, tail: List[Tree[A]])
  val treeTraverse = new Traverse[Tree] {
    def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit
        g: Applicative[G]
    ): G[Tree[B]] = {
      val gb: G[B] = f(fa.head)
      val gltb: G[List[Tree[B]]] = g.sequence(fa.tail.map(traverse(_)(f)))
      g.map2(gb, gltb)(((b: B), (ltb: List[Tree[B]])) => Tree(b, ltb))
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

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = ???

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] =
      st flatMap f
  }

  // 12.20
  def composeM[G[_], H[_]](implicit
      G: Monad[G],
      H: Monad[H],
      T: Traverse[H]
  ) = new Monad[({ type f[x] = G[H[x]] })#f] {
    override def unit[A](a: => A): G[H[A]] =
      G.unit(H.unit(a))
    override def flatMap[A, B](gha: G[H[A]])(f: A => G[H[B]]): G[H[B]] = {
      val y: H[A] => G[H[H[B]]] = ha => T.traverse(ha)(f)
      val x: H[A] => G[H[B]] = ha => G.map(y(ha))(H.join(_))
      G.flatMap(gha)(x)
    }
  }
}

object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
