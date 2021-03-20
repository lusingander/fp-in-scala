package fpinscala

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.testing.SGen

object Main extends App {
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  Prop.run(maxProp)
}
