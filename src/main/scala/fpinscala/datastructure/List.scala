package fpinscala.datastructure

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, xs) => xs
    case Nil         => Nil
  }

  // 3.3
  def setHead[A](as: List[A], a: A): List[A] = Cons(a, tail(as))

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l else drop(tail(l), n - 1)

  // 3.5
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if p(x) => dropWhile(xs, p)
      case _                   => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil         => a2
      case Cons(x, xs) => Cons(x, append(xs, a2))
    }

  // 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil          => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x + product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, List(as.tail: _*))
}
