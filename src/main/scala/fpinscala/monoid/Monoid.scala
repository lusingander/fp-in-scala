package fpinscala.monoid

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.parallelism.Par

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = List()
  }

  // 10.1
  val IntAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }
  val IntMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = (a1, a2) match {
      case (Some(_), _) => a1
      case (_, None)    => a1
      case (None, _)    => a2
    }
    def zero = None
  }

  // 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    def zero = identity
  }

  // 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val gen3 = gen.listOfN(3).map { case x :: y :: z :: _ => (x, y, z) }
    Prop.forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a) &&
    Prop.forAll(gen3) { case (x, y, z) =>
      m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    }
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // 10.6
  def foldRight_[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(a => b => (f(a, b)))(z)
  }
  def foldLeft_[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val m = new Monoid[B => B] {
      def op(g1: B => B, g2: B => B) = g2 compose g1
      def zero = identity
    }
    foldMap(as, m)(a => b => (f(b, a)))(z)
  }

  // 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.length match {
      case 0 => m.zero
      case 1 => f(v.head)
      case _ =>
        val (v1, v2) = v.splitAt(v.length / 2)
        m.op(
          foldMapV(v1, m)(f),
          foldMapV(v2, m)(f)
        )
    }
  }

  // 10.8
  def par[A](m: Monoid[A]): Monoid[Par.Par[A]] = new Monoid[Par.Par[A]] {
    def op(p1: Par.Par[A], p2: Par.Par[A]) = Par.map2(p1, p2)(m.op)
    def zero = Par.unit(m.zero)
  }
  // def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par.Par[B] =
  //   foldMapV(v, par(m))(a => Par.unit(f(a)))

  // 10.9
  def isOrdered(s: IndexedSeq[Int]): Boolean =
    ???
}
