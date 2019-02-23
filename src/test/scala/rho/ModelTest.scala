package rho

import org.scalatest.FunSuite

class ModelTest extends FunSuite {

  import Model.satisfies

  test("trivial verity") {
    satisfies(Zero, True)
  }

  test("double Negation") {
    satisfies(Zero, Negation(Negation(True)))
  }

  val n1 = Quote(Zero)

  test("trivial descent") {
    satisfies(Drop(n1), Descent(n1))
  }

  test("Zero mixes with ZeroF") {
    satisfies(Zero, Mixture(List(ZeroF)))
  }

  test("drop doesn't mix with ZeroF") {
    satisfies(Drop(n1), Negation(Mixture(List(ZeroF))))
  }
}
