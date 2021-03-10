package fpinscala.datastructure

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  // 3.26
  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value)         => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  // 3.27
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)             => 0
      case Branch(left, right) => (depth(left) max depth(right)) + 1
    }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  // 3.29
  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B =
    t match {
      case Leaf(n)             => z(n)
      case Branch(left, right) => f(fold(left)(z)(f), fold(right)(z)(f))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((n, m) => 1 + n + m)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(n => n)((n, m) => n max m)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((n, m) => (n max m) + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(n => Leaf(f(n)))((n, m) => Branch(n, m))

  // def foldInt[A](t: Tree[A])(z: A => Int)(f: (Int, Int) => Int): Int =
  //   t match {
  //     case Leaf(n)             => z(n)
  //     case Branch(left, right) => f(foldInt(left)(z)(f), foldInt(right)(z)(f))
  //   }

  // def foldInt(t: Tree[Int])(z: Int => Int)(f: (Int, Int) => Int): Int =
  //   t match {
  //     case Leaf(n)             => z(n)
  //     case Branch(left, right) => f(foldInt(left)(z)(f), foldInt(right)(z)(f))
  //   }

  // def sizeByFoldInt[A](t: Tree[A]): Int =
  //   foldInt(t)(_ => 1)((n, m) => 1 + n + m)

  // def maximumByFoldInt(t: Tree[Int]): Int =
  //   foldInt(t)(n => n)((n, m) => n max m)

  def sample1: Tree[Int] =
    Branch(
      Branch(
        Leaf(3),
        Leaf(4)
      ),
      Branch(
        Leaf(6),
        Branch(
          Leaf(1),
          Leaf(5)
        )
      )
    )

  def sample2: Tree[Int] =
    Branch(
      Branch(
        Leaf(3),
        Leaf(7)
      ),
      Branch(
        Leaf(6),
        Branch(
          Leaf(1),
          Branch(
            Leaf(2),
            Leaf(4)
          )
        )
      )
    )

  def sample3: Tree[Int] =
    Branch(
      Branch(
        Leaf(3),
        Leaf(4)
      ),
      Branch(
        Leaf(6),
        Leaf(5)
      )
    )
}
