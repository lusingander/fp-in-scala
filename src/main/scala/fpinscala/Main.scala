package fpinscala

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.testing.SGen
import fpinscala.monoid.Monoid

object Main extends App {
  val ints = Gen.choose(-100, 100)
  val intAdditionProp = Monoid.monoidLaws(Monoid.IntAddition, ints)
  Prop.run(intAdditionProp)
  val intMultiplicationProp = Monoid.monoidLaws(Monoid.IntMultiplication, ints)
  Prop.run(intMultiplicationProp)

  val booleanOrProp = Monoid.monoidLaws(Monoid.booleanOr, Gen.boolean)
  Prop.run(booleanOrProp)
  val booleanAndProp = Monoid.monoidLaws(Monoid.booleanAnd, Gen.boolean)
  Prop.run(booleanAndProp)
}
