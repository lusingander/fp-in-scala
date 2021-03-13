package fpinscala.state

// 6.10
case class State[S, +A](run: S => (A, S)) {

  type Rand[A] = State[RNG, A]

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, n1) = run(s)
      f(a).run(n1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](t: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => t.map(b => f(a, b)))
}

object State {

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(State.unit[S, List[A]](List()))((s, l) => s.map2(l)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

// 6.11

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  // Input => Machine => Machine
  def process(input: Input): Machine => Machine =
    machine => {
      (input, machine) match {
        case (Coin, Machine(true, x, y)) if x > 0 =>
          Machine(false, x, y + 1)
        case (Turn, Machine(false, x, y)) =>
          Machine(true, x - 1, y)
        case (Coin, Machine(false, x, y)) =>
          machine
        case (Turn, Machine(true, x, y)) =>
          machine
      }
    }

  // List[Input] => Machine => ((Int, Int), Machine)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val processes = inputs.map(process(_)) // List[Machine => Machine]
    for {
      _ <- State.sequence(processes.map(State.modify(_)))
      p <- State.get
    } yield (p.coins, p.candies)
  }
}
