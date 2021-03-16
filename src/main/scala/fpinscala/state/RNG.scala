package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, next) = s(rng)
      (f(a), next)
    }

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, na) = ra(rng)
      val (b, nb) = rb(na)
      (f(a, b), nb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, l) => map2(f, l)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  val int: Rand[Int] = _.nextInt

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, next) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, next) else nonNegativeLessThan(n)(rng)
  }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, next) = f(rng)
      g(a)(next)
    }

  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // 6.9
  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  // 6.5
  def double2: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1))

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt
    val m = if (n < 0) -(n + 1) else n
    (m, next)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, next) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble + 1), next)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, next1) = rng.nextInt
    val (d, next2) = RNG.double(next1)
    ((n, d), next2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), next) = RNG.intDouble(rng)
    ((d, n), next)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, n1) = RNG.double(rng)
    val (d2, n2) = RNG.double(n1)
    val (d3, n3) = RNG.double(n2)
    ((d1, d2, d3), n3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) {
      (List(), rng)
    } else {
      val (n, next1) = rng.nextInt
      val (l, next2) = ints(count - 1)(next1)
      (n :: l, next2)
    }
  }
}
