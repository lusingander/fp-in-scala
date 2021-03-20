package fpinscala.parsing

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>

  def or[A](a1: Parser[A], a2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]) =
    ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  // def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  // def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  // 9.1
  def map2[A, B, C](
      p1: Parser[A],
      p2: => Parser[B]
  )(
      f: (A, B) => C
  ): Parser[C] =
    map(product(p1, p2))(ab => f(ab._1, ab._2))
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List()))

  // 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)

  // 9.6
  def na: Parser[Unit] =
    for {
      n <- regex("[0-9]".r)
      _ <- listOfN(n.toInt, char('a'))
    } yield ()

  // 9.7
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p1.flatMap(a => p2.map(b => (a, b)))
  def map2_[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] =
    p1.flatMap(a => p2.map(b => f(a, b)))

  // 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B) = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)

    def product[B](q: => Parser[B]) = self.product(p, q)
    def **[B](q: => Parser[B]) = product(q)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)
  }
}
