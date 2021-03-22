package fpinscala.laziness

sealed trait Stream[+A] {

  def headOption: Option[A] =
    this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

  // 5.1
  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // 5.2
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _                   => Stream.empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0  => t().drop(n - 1)
      case Cons(_, _) if n == 0 => this
      case _                    => Stream.empty
    }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _                    => Stream.empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a, b) =>
      if (p(a)) Stream.cons(a, b) else Stream.empty
    )

  // 5.6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a, b) =>
      if (p(a)) Stream.cons(a, b) else b
    )

  def append[AA >: A](s: Stream[AA]): Stream[AA] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty: Stream[B])((a, b) => f(a).append(b))

  // 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    Stream
      .zipAll(this, s)
      .takeWhile(t => t._2.isDefined)
      .forAll(t => t._1 == t._2)

  // 5.15
  def tails: Stream[Stream[A]] =
    Stream
      .unfold(this) {
        case s @ Cons(h, t) => Some((s, t()))
        case Empty          => None
      }
      .append(Stream(Stream.empty)) // :(

  // 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((a, sb) => Stream.cons(f(a, sb.headOption.get), sb))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // 5.10
  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, go(b, a + b))
    }
    go(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  // 5.12
  def fibs2(): Stream[Int] =
    unfold((0, 1))(p => Some((p._1, (p._2, p._1 + p._2))))

  def from2(n: Int): Stream[Int] =
    unfold(n)(m => Some((m, m + 1)))

  def constant2[A](a: A): Stream[A] =
    unfold(())(_ => Some((a, ())))

  def ones2(): Stream[Int] = constant2(1)

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // 5.13
  def map[A, B](s: Stream[A])(f: A => B): Stream[B] =
    unfold(s)(ss => ss.headOption.map(f).map(h => (h, ss.drop(1))))

  def map2[A, B](s: Stream[A])(f: A => B): Stream[B] =
    unfold(s) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None
    }

  def take[A](s: Stream[A])(n: Int): Stream[A] =
    unfold((s, n)) { case (ss, nn) =>
      ss.headOption
        .map(h => (h, (ss.drop(1), nn - 1)))
        .filter { case (_, (_, m)) => m >= 0 }
    }

  def take2[A](s: Stream[A])(n: Int): Stream[A] =
    unfold((s, n)) {
      case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
      case _                        => None
    }

  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(s) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((a, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case (_, _)                       => None
    }

  def zipAll[A, B](a: Stream[A], b: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((a, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (_, _)                => None
    }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
