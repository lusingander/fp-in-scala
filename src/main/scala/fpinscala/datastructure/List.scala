package fpinscala.datastructure

import scala.annotation.tailrec

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
  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)((x, y) => x + y)

  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)((x, y) => x * y)

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // 3.11
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

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
