package fpinscala.testing

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

import fpinscala.parallelism.Par
import fpinscala.state.RNG
import fpinscala.state.SimpleRNG
import fpinscala.state.State

import Prop._
import Gen.**

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

  def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(failure, successes) =>
        println(s"! Falsified after $successes passed tests: \n $failure")
      case Passed =>
        println(s"OK, passed $testCases tests.")
      case Proved =>
        println("OK, proved property.")
    }

  val ES = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
  )
  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val q = Par.unit(2)
    p(ES).get == q(ES).get
  }
  def equal[A](p: Par.Par[A], q: Par.Par[A]): Par.Par[Boolean] =
    Par.map2(p, q)(_ == _)
  val p3 = Prop.check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }
  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )
  def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par.Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val p2_ = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(identity), n))

  // 8.17
  val pf = forAllPar(pint)(n => equal(Par.fork(n), n))

  // def check(p: => Boolean): Prop = {
  //   lazy val result = p
  //   forAll(Gen.unit(()))(_ => result)
  // }
  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

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
          case Passed | Proved => p.run(m, n, rng)
          case f               => f
        }
      }
    }

  def ||(p: Prop): Prop =
    Prop { (m, n, rng) =>
      {
        run(m, n, rng) match {
          case Falsified(failure, _) => p.tag(failure).run(m, n, rng)
          case p                     => p
        }
      }
    }

  def tag(s: String) =
    Prop { (m, n, rng) =>
      {
        run(m, n, rng) match {
          case Falsified(failure, successes) =>
            Falsified(s + "\n" + failure, successes)
          case x => x
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
    failure: FailedCase,
    successes: SuccessCount
) extends Result {
  def isFalsified: Boolean = true
}

case object Proved extends Result {
  def isFalsified: Boolean = false
}

// trait Prop {
//   // def check: Boolean
//   // // 8.3
//   // def &&(p: Prop): Prop = new Prop {
//   //   def check = Prop.this.check && p.check
//   // }
//   def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]
// }

case class Gen[+A](sample: State[RNG, A]) {

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(this.listOfN(_))

  def listOfN(n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(sample)))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    map2(g)((_, _))
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

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  // 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))
}

object SGen {

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))
}
