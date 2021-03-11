package fpinscala.error

sealed trait Option[+A] {

  // 4.1
  def map[B](f: A => B): Option[B] =
    this match {
      case None    => None
      case Some(v) => Some(f(v))
    }

  // def flatMap[B](f: A => Option[B]): Option[B] =
  //   this match {
  //     case None    => None
  //     case Some(v) => f(v)
  //   }
  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None    => default
      case Some(v) => v
    }

  // def orElse[B >: A](ob: => Option[B]): Option[B] =
  //   this match {
  //     case None    => ob
  //     case Some(_) => this
  //   }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(o => Some(o)).getOrElse(ob)

  // def filter(f: A => Boolean): Option[A] =
  //   this match {
  //     case None    => None
  //     case Some(v) => if (f(v)) this else None
  //   }
  // def filter(f: A => Boolean): Option[A] =
  //   if (this.map(f).getOrElse(false)) this else None
  def filter(f: A => Boolean): Option[A] =
    flatMap(v => if (f(v)) Some(v) else None)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    Option.mean(xs).flatMap(m => Option.mean(xs.map(x => Math.pow(x - m, 2))))
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def lift[A, B](f: A => B): Option[A] => Option[B] = v => v.map(f)

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  def map2_[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for { aa <- a; bb <- b } yield f(aa, bb)

  // 4.4
  // def sequence[A](as: List[Option[A]]): Option[List[A]] =
  //   as match {
  //     case Nil     => Some(Nil)
  //     case x :: xs => sequence(xs).flatMap(l => x.map(_ :: l))
  //   }
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil): Option[List[A]])((x, l) =>
      l.flatMap(ll => x.map(_ :: ll))
    )
  // def sequence2[A](a1: Option[A], a2: Option[A]): Option[List[A]] =
  //   a1.flatMap(aa1 => a2.map(aa2 => List(aa1, aa2)))
  // def sequence3[A](
  //     a1: Option[A],
  //     a2: Option[A],
  //     a3: Option[A]
  // ): Option[List[A]] =
  //   a1.flatMap(aa1 => a2.flatMap(aa2 => a3.map(aa3 => List(aa1, aa2, aa3))))

  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((x, l) => map2(f(x), l)(_ :: _))
}
