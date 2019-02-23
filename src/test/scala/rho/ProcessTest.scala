package rho

object ProcessTest {
  import Process._

  def main(args: Array[String]): Unit = {
    println("Hello?")
    testZero()
    val (actual, expected) = parFlattens()
    println(expected)
    println(actual)
    assert(actual == expected)
    println("All tests passed")
  }
  val n1 = S("n1")
  val p0: Process = zero
  val p1 = Lift(n1, Zero)
  def testZero(): Unit = {
    assert(zero == zero)
  }
  def parFlattens():(Process, Process) = {
    val actual = par(Par(List(p0, p1)), Par(List(p1, p0)))
    val expected = Par(List(p0, p1, p1, p0))
    (actual, expected)
  }
}
