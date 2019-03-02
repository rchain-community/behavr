package rho

import scala.annotation.tailrec

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

  def parstar(proclist: List[Process]): Process =
    proclist match {
      case Nil => Zero
      case proclisthd :: proclisttl =>
        proclisttl.foldLeft(proclisthd)(par)
    }

  val nilp: List[Process] = Nil

  // aka structurallyEquivalent
  def equivalent(proc1: Process, proc2: Process): Boolean = (proc1, proc2) match {
    // the empty par is 0
    case (Zero, Par(Nil)) => true
    case (Par(Nil), Zero) => true

    // 0 is structurally equivalent to 0case0|...|0 *)
    case (Zero, Par(proclisthd :: proclisttl)) =>
      equivalent(proc1, proclisthd) && equivalent(proc1, Par(proclisttl))
    case (Par(proclisthd :: proclisttl), Zero) =>
      equivalent(Zero, Par(proclisthd :: proclisttl))

    // structural equivalence includes alpha equivalence
    case (Input(Action(nsubj1, nobj1), cont1), Input(Action(nsubj2, nobj2), cont2)) =>
      (Name.equivalent(nsubj1, nsubj2) // (nsubj1 = nsubj2) replaced with name equality
        && equivalent(cont1, syntacticSubstitution(cont2, nobj1, nobj2)))


    // par is commutative and associative *)
    case (Par(proclisthd1 :: proclisttl1), Par(proclist2)) =>
      proclist2.partition { proc => equivalent(proclisthd1, proc) } match {
        case (Nil, _) => false
        case (eqhd, eqtl) =>
          eqhd.foldLeft((false, nilp, eqhd.tail))((rejects, proc) =>
            rejects match {
              case (false, r, l) =>
                if (equivalent(parstar(r ::: l ::: eqtl), Par(proclisttl1)))
                  (true, r, l)
                else (false, r ::: List(proc), l.tail)
              case (true, r, l) => (true, r, l)
            }
          ) match {
            case (ans, _, _) => ans
          }
      }

    case (Par(proclist1), Par(proclisthd2 :: proclisttl2)) =>
      equivalent(
        Par(proclisthd2 :: proclisttl2),
        Par(proclist1))

    // 0 is the identity for par *)
    case (_, Par(proclist)) =>
      proclist.partition(proc => equivalent(proc1, proc)) match {
        case (Nil, _) => false
        case (List(_), procs) => equivalent(Zero, Par(procs))
        case (_ :: _, _) => false
      }
    case (Par(proclist), _) =>
      equivalent(proc2, Par(proclist))

    // structural equivalence includes syntactic equality
    case _ => proc1 == proc2
  }


  def calculateNextName(proc: Process): Name = proc match {
    case Zero => Quote(Zero)
    case Input(Action(Quote(psubj), Quote(pobj)), cont) =>
      Quote(parstar(List(psubj, pobj, cont)))
    case Lift(Quote(psubj), cont) =>
      Quote(par(psubj, cont))
    case Drop(Quote(p)) => Quote(par(p, p))
    case Par(Nil) => Quote(Zero)
    case Par(proclisthd :: proclisttl) =>
      Quote(proclisttl.foldLeft(proclisthd)(par))
  }


  def syntacticSubstitution(proc: Process, nsource: Name, ntarget: Name): Process =
    proc match {
      case Zero => Zero
      case Input(Action(nsubj, nobj), cont) =>
        val obj =
          if (Name.equivalent(nobj, ntarget)) // ( nobj = ntarget ) replaced with name equality
            calculateNextName(Input(Action(nsubj, nobj), cont))
          else nobj

        Input(Action(if (Name.equivalent(nsubj, ntarget)) // ( nsubj = ntarget ) replaced with name equality
          nsource else nsubj, obj)
          ,
          syntacticSubstitution(
            if (Name.equivalent(nobj, ntarget)) // ( nobj = ntarget ) replaced with name equality *)
              syntacticSubstitution(cont, obj, nobj) else cont,
            nsource,
            ntarget)
        )
      case Lift(nsubj, cont) =>
        Lift(
          if (Name.equivalent(nsubj, ntarget)) // (nsubj = ntarget) replaced with name equality *)
            nsource
          else nsubj,
          syntacticSubstitution(cont, nsource, ntarget)
        )
      case Drop(n) =>
        Drop(
          if (Name.equivalent(n, ntarget)) // (n = ntarget) replaced with name equality *)
            nsource
          else n
        )
      case Par(proclist) =>
        Par(proclist.map(proc => syntacticSubstitution(proc, nsource, ntarget)))
    }

  def gt(p1: Process, p2: Process): Boolean = (p1, p2) match {
    case (Zero, _) => false
    case (Input(_, _), Zero) => true
    case (
      Input(Action(Quote(s1), Quote(o1)), c1),
      Input(Action(Quote(s2), Quote(o2)), c2)) => gt(s1, s2) || gt (o1, o2) || gt(c1, c2)
    case (Input(_, _), Lift(_, _)) => false
    case (Input(_, _), Drop(_)) => false
    case (Input(_, _), Par(_)) => false
    case (Lift(_, _), Zero) => true
    case (Lift(_, _), Input(_, _)) => true
    case (Lift(_, _), Drop(_)) => false
    case (
      Lift(Quote(s1), c1),
      Lift(Quote(s2), c2)) => gt(s1, s2) || gt(c1, c2)
    case (Lift(_, _), Par(_)) => false
    case (Drop(_), _) => false // not normalized
    case (Par(_), Zero) => true
    case (Par(_), Input(_, _)) => true
    case (Par(_), Lift(_, _)) => true
    case (Par(_), Drop(_)) => true
    case (Par(Nil), Par(Nil)) => false
    case (Par(Nil), Par(_)) => false
    case (Par(_), Par(Nil)) => true
    case (Par(pp1 :: ps1), Par(pp2 :: ps2)) => gt(pp1, pp2) && gt(Par(ps1), Par(ps2))
    case oops =>
      println("@@@oops!" + oops.toString())
      false
   }

  val nilProcs: List[Process] = Nil

  def normalize(p: Process): Process = p match {
    case Zero => Zero
    case Input(Action(Quote(subj), Quote(obj)), cont) =>
      val subjn = normalize(subj)
      val objn = normalize(obj)
      val contn = normalize(cont)
      Input(Action(Quote(subjn), Quote(objn)), contn)
    case Lift(Quote(subj), cont) =>
      val subjn = normalize(subj)
      val contn = normalize(cont)
      Lift(Quote(subjn), contn)
    case Drop(Quote(pp)) => normalize(pp)
    case Par(ps) =>
      val parts = ps.map(normalize).sortWith(gt).distinct
      parts match {
        case Nil => Zero
        case List(pp) => pp
        case _ => Par(parts)
      }
    case _ =>
      println("@@@@oops!" + p.toString)
      p
  }
}
