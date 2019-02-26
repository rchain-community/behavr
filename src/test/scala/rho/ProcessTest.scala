package rho

import org.scalatest.FunSuite

class ProcessTest extends FunSuite {
  import Process._

  val n1 = Quote(Par(List(Zero, Zero)))
  val p0: Process = zero
  val p1 = Lift(n1, Zero)

  test("Zero is equivalent to itself") {
    assert(equivalent(Zero, Zero))
  }

  test("parFlattens") {
    val actual = par(Par(List(p0, p1)), Par(List(p1, p0)))
    val expected = Par(List(p0, p1, p1, p0))
    assert(actual === expected)
  }
}
