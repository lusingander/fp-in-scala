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
}
