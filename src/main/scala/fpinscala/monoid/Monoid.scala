package fpinscala.monoid

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.parallelism.Par
import fpinscala.datastructure.Tree

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
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
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

  // 10.16
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)) = (ma.op(x._1, y._1), mb.op(y._2, y._2))
      def zero = (ma.zero, mb.zero)
    }

  def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(x: Map[K, V], y: Map[K, V]) =
        (x.keySet ++ y.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(
            k,
            mv.op(x.getOrElse(k, mv.zero), y.getOrElse(k, mv.zero))
          )
        }
      def zero = Map[K, V]()
    }

  // 10.17
  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B) = a => mb.op(f(a), g(a))
      def zero = _ => mb.zero
    }

  // 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

// 10.10
object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(x: WC, y: WC): WC = (x, y) match {
      case (Stub(s1), Stub(s2))                 => Stub(s1 + s2)
      case (Stub(s), Part(l, w, r))             => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s))             => Part(l, w, r + s)
      case (Part(l1, w1, ""), Part("", w2, r2)) => Part(l1, w1 + w2, r2)
      case (Part(l1, w1, _), Part(_, w2, r2))   => Part(l1, w1 + w2 + 1, r2)
    }
    def zero: WC = Stub("")
  }

  // 10.11
  def wc(in: String): Int = {
    val toWc = (c: Char) => if (c == ' ') Part("", 0, "") else Stub(c.toString)
    val c = (s: String) => if (s.isEmpty) 0 else 1
    Monoid.foldMapV(in, wcMonoid)(toWc) match {
      case Stub(s)       => c(s)
      case Part(l, w, r) => c(l) + w + c(r)
    }
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A

  // 10.15
  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List[A]())(_ :: _)
}

object Foldable {

  // 10.12
  object ListFoldable extends Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      Monoid.foldMap(as, mb)(f)
    def concatenate[A](as: List[A])(m: Monoid[A]): A =
      Monoid.concatenate(as, m)
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      Monoid.foldMapV(as, mb)(f)
    def concatenate[A](as: IndexedSeq[A])(m: Monoid[A]): A =
      as.foldLeft(m.zero)(m.op)
  }

  object StreamFoldable extends Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
    def concatenate[A](as: Stream[A])(m: Monoid[A]): A =
      as.foldLeft(m.zero)(m.op)
  }

  // 10.13
  object TreeFoldable extends Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(a => b => (f(a, b)))(Monoid.endoMonoid[B])(z)
    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      val m = new Monoid[B => B] {
        def op(g1: B => B, g2: B => B) = g2 compose g1
        def zero = identity
      }
      foldMap(as)(a => b => (f(b, a)))(m)(z)
    }
    def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      Tree.fold(as)(f)(mb.op)
    def concatenate[A](as: Tree[A])(m: Monoid[A]): A =
      foldMap(as)(identity)(m)
  }

  // 10.14
  object OptionFoldable extends Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as match {
        case None    => z
        case Some(a) => f(a, z)
      }
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case None    => z
        case Some(a) => f(z, a)
      }
    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None    => mb.zero
        case Some(a) => f(a)
      }
    def concatenate[A](as: Option[A])(m: Monoid[A]): A =
      as match {
        case None    => m.zero
        case Some(a) => a
      }
  }
}
