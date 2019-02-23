package rho

/**
  * RhoCalculus Process
  *
  * ref:
  * A Reflective Higher-order Calculus,
  * L.G. Meredith, Matthias Radestock 2005
  * https://doi.org/10.1016/j.entcs.2005.05.016.
  *
  * transcribed from https://github.com/leithaus/rhocaml/blob/master/rho.ml
  * da884fb  on Jun 19, 2016 Meredith Gregory
  */
sealed trait Process

case object Zero extends Process
case class Input(act: Action, cont: Process) extends Process
case class Lift(n: Name, proc: Process) extends Process
case class Drop(n: Name) extends Process
case class Par(ps: List[Process]) extends Process

case class Action(nsubj: Name, nobj: Name)

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

  // aka structurallyEquivalent
  def equivalent(proc1: Process, proc2: Process): Boolean = ( proc1, proc2 ) match {
    // the empty par is 0
    case ( Zero, Par( Nil ) ) => true
    case ( Par( Nil ), Zero ) => true

    // 0 is structurally equivalent to 0|0|...|0 *)
    case ( Zero, Par( proclisthd :: proclisttl ) ) =>
      equivalent(proc1, proclisthd) && equivalent(proc1, Par( proclisttl ))
    // ... TODO: more cases
    // structural equivalence includes syntactic equality
    case _ => proc1 == proc2
  }
}
