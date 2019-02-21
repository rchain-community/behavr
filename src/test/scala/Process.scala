sealed trait Process

case object Zero extends Process
case class Input(act: Action, cont: Process) extends Process
case class Lift(n: Name, proc: Process) extends Process
case class Drop(n: Name) extends Process
case class Par(ps: List[Process]) extends Process

case class Action(nsubj: Name, nobj: Name)

sealed trait Name
case class Quote(p: Process) extends Name
case class S(s: String) extends Name


object ProcessUtil {
  def zero = Zero
  def guard(nsubj: Name, nobj: Name) = Action(nsubj, nobj)
  def input(nsubj: Name, nobj: Name, cont: Process) =
    Input( Action( nsubj, nobj ), cont )

  def par(proc1: Process, proc2: Process) :Par =
      ( proc1, proc2 ) match {
      case ( Par( proclist1 ), Par( proclist2 ) ) => Par( proclist1 ++ proclist2 )
      case ( Par( proclist ), proc ) => Par( proclist ++ List( proc ) )
      case ( proc, Par( proclist ) ) => Par( proc :: proclist )
      case ( p1, p2 ) => Par( List( p1, p2 ) )
    }

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
  val p0 = zero
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
