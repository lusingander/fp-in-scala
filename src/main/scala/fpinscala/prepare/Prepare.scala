package fpinscala.prepare

import scala.annotation.tailrec

object Prepare {

  def factorial(n: Int) = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc else go(n - 1, acc * n)
    }
    go(n, 1)
  }

  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, a: Int, b: Int): Int = {
      if (n <= 1) a else go(n - 1, b, a + b)
    }
    go(n, 0, 1)
  }

  def bad_fib(n: Int): Int = {
    if (n <= 1) {
      0
    } else if (n == 2) {
      1
    } else {
      bad_fib(n - 1) + bad_fib(n - 2)
    }
  }

  def findFirst[A](as: Array[A], pred: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) {
        -1
      } else if (pred(as(n))) {
        n
      } else {
        loop(n + 1)
      }
    }
    loop(0)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) {
        true
      } else if (!ordered(as(n), as(n + 1))) {
        false
      } else {
        loop(n + 1)
      }
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
