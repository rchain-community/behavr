package rho

import org.scalatest.FunSuite

class ProcessTest extends FunSuite {
  import Process._

  val n1 = S("n1")
  val p0: Process = zero
  val p1 = Lift(n1, Zero)

  test("Zero") {
    assert(zero === zero)
  }
  test("parFlattens") {
    val actual = par(Par(List(p0, p1)), Par(List(p1, p0)))
    val expected = Par(List(p0, p1, p1, p0))
    assert(actual === expected)
  }
}
