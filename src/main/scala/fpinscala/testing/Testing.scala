package fpinscala.testing

import fpinscala.state.State
import fpinscala.state.RNG

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (m, n, rng) =>
    val casesPerSize = (n + (m - 1)) / m
    val props: Stream[Prop] =
      Stream.from(0).take((n min m) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props
      .map(p => Prop { (m, _, rng) => p.run(m, casesPerSize, rng) })
      .toList
      .reduce(_ && _)
    prop.run(m, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    {
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map { case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString(), i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n" +
      s" ${e.getStackTrace.mkString("\n")}"
}

case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Result) {

  // // 8.9
  // def &&(p: Prop): Prop =
  //   Prop { (n, rng) =>
  //     {
  //       run(n, rng) match {
  //         case Passed => p.run(n, rng)
  //         case f      => f
  //       }
  //     }
  //   }

  // def ||(p: Prop): Prop =
  //   Prop { (n, rng) =>
  //     {
  //       run(n, rng) match {
  //         case Falsified(_, _) => p.run(n, rng)
  //         case p               => p
  //       }
  //     }
  //   }

  def &&(p: Prop): Prop =
    Prop { (m, n, rng) =>
      {
        run(m, n, rng) match {
          case Passed => p.run(m, n, rng)
          case f      => f
        }
      }
    }

  def ||(p: Prop): Prop =
    Prop { (m, n, rng) =>
      {
        run(m, n, rng) match {
          case Falsified(_, _) => p.run(m, n, rng)
          case p               => p
        }
      }
    }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(
    failure: Prop.FailedCase,
    successes: Prop.SuccessCount
) extends Result {
  def isFalsified: Boolean = true
}

// trait Prop {
//   // def check: Boolean
//   // // 8.3
//   // def &&(p: Prop): Prop = new Prop {
//   //   def check = Prop.this.check && p.check
//   // }
//   def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]
// }

case class Gen[A](sample: State[RNG, A]) {

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  // 8.4
  def choose(start: Int, endExclusive: Int): Gen[Int] =
    Gen(
      State(
        RNG.map(RNG.nonNegativeLessThan(endExclusive - start))(_ + start)
      )
    )

  // 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeLessThan(2)).map(i => if (i == 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  // 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    zeroToOne.flatMap(d => if ((g1._2 / (g1._2 + g2._2)) < d) g1._1 else g2._1)

  def zeroToOne: Gen[Double] = Gen(State(RNG.double2))
}

case class SGen[A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  // 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
}

object SGen {

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))
}
