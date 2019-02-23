package rho

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


object Process {
  def zero: Process = Zero

  def guard(nsubj: Name, nobj: Name) = Action(nsubj, nobj)

  def input(nsubj: Name, nobj: Name, cont: Process) =
    Input(Action(nsubj, nobj), cont)

  def par(proc1: Process, proc2: Process): Par =
    (proc1, proc2) match {
      case (Par(proclist1), Par(proclist2)) => Par(proclist1 ++ proclist2)
      case (Par(proclist), proc) => Par(proclist ++ List(proc))
      case (proc, Par(proclist)) => Par(proc :: proclist)
      case (p1, p2) => Par(List(p1, p2))
    }
}
