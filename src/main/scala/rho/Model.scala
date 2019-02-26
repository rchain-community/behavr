package rho

import scala.annotation.tailrec

class Model {

}

class InvalidModelFormulaSetList extends RuntimeException


object Model {

  import Formula.logicalSubstitution
  import Process.syntacticSubstitution

  def satisfies(proc: Process, form: Formula): Boolean = form match {
    case True => true
    case  ZeroF => Process.equivalent(proc, Zero)
    case Negation(nForm) => !satisfies(proc, nForm)
    case Conjunction(Nil) => false
    case Conjunction(hForm :: rForm) =>
      (hForm :: rForm).forall {
        satisfies(proc, _)
      }
    case Mixture(Nil) => Process.equivalent(proc, Zero)
    case Mixture(hForm :: rForm) => proc match {
      case Par(Nil) => (hForm :: rForm).forall {
        _ == ZeroF
      }
      case Par(hProc :: rProc) => parMix(hProc, rProc, hForm, rForm)
      case _ => false
    }
    case Descent(m) => // LGM: should we require exact match in this case and let the user specify the true?
      proc match {
        case Drop(n) => Name.equivalent(m, n)
        case Par(hProc :: rProc) =>
          if (!satisfies(hProc, Descent(m))) satisfies(Par(rProc), Descent(m))
          else true
        case _ => false
      }
    case Elevation(i, eForm) =>
      proc match {
        case Lift(nsubj, lProc) => nominallySatisfies(nsubj, i) && satisfies(lProc, eForm)
        case Par(hProc :: rProc) =>
          if (!satisfies(hProc, Elevation(i, eForm)))
            satisfies(Par(rProc), Elevation(i, eForm))
          else true
        case _ => false
      }
    case Activity(Condition(i, n), aForm) =>
      proc match {
        case Input(Action(nsubj, nobj), iProc) =>
          nominallySatisfies(nsubj, i) && {
            val ntarget = Quote(Par(List(Lift(nobj, Zero), Lift(n, Zero))))
            satisfies(syntacticSubstitution(iProc, nobj, ntarget), logicalSubstitution(aForm, n, ntarget))
          }
        case _ => false
      }
  }

  def nominallySatisfies(n: Name, i: Indicator): Boolean =
    n match {
      case Quote(p) =>
        i match {
          case Quotation(f) =>
            satisfies(p, f)
          case Naming(Quote(q)) => Name.equivalent(Quote(p), Quote(q))
        }
    }


  /** *********************************************************************************************************************)
    * (* the core algorithm for this case is to recognize when there are too many formulae competing for available processes *)
    * (* the converse, i.e. more processes than formula, is ok when there is a true in the mix of formulae                   *)
    * (* the complexity of this algorithm is bound to be unacceptable because there is no attempt to take advantage of lucky *)
    * (* breaks in the given configuration of formulae and processes; all processes in the mix that satisfy a given formula  *)
    * (* will be computed rather than trying to find some configuration that satisfies the constraints                       *)
    * (* however, we eliminate backtracking by doing this so the complexity analysis is a tad subtle                         *)
    * ( ***********************************************************************************************************************/
  def parMix(hProc: Process, rProc: List[Process], hForm: Formula, rForm: List[Formula]): Boolean = {
    val ntForm = (hForm :: rForm).partition(_ == True)
    ntForm match {
      case
        (ltform, Nil) => true // form looks like true | ... | true *)
      case (ltform, hntForm :: rntForm) => // form looks like phi_0 | ... | phi_n| true | ... | true *)
        @tailrec
        def aloop(pfMatch: List[(Set[Process], List[Formula])]): Boolean = pfMatch match {
          case Nil => true // no failing ( { p_j }, [ phi_k ] ) pair *)
          case hpfMatch :: rpfMatch =>
            hpfMatch match {
              case (pSet, fList) =>
                val fls = fList.size
                val pss = pSet.size
                if (fls > pss) false // ( { p_j }, [ phi_k ] ) with k > j *)
                else if (pss > fls)
                  ltform match {
                    case Nil => false // ( { p_j }, [ phi_k ] ) with j > k and no true to absorb leftover p *)
                    case ne => aloop(rpfMatch) // ( { p_j }, [ phi_k ] ) with j > k and true to absorb leftover p *)
                  }
                else aloop(rpfMatch) // ( { p_j }, [ phi_k ] ) with j = k *)
            }
        }

        def floop(lForm: List[Formula], acc: List[(Set[Process], List[Formula])]): Boolean =
          lForm match {
            case Nil => aloop(acc) // we've come to the end of the list of phi_i *)
            case hlForm :: rlForm => // some formulae to process *)
              (hProc :: rProc).partition(p => satisfies(p, hlForm)) match {
                case (Nil, nProc) => false // no process matching hlForm *)
                case (hmProc :: rmProc, nProc) => // some processes matching hlForm *)
                  val pSet = rmProc.foldLeft(Set(hmProc))((acc, proc) => acc ++ Set(proc))
                  val aNil: List[(Set[Process], List[Formula])] = Nil
                  val start = (List((pSet, List(hlForm))), aNil)
                  val cacc = acc.foldLeft(start) { (facc, hacc) =>
                    hacc match {
                      case (hapSet, hafSet) =>
                        facc match {
                          case (List((gpSet, gfSet)), ansSet) =>
                            val ipSet = gpSet.intersect(hapSet)
                            if (ipSet.isEmpty)
                            // so far only pSet |= hlForm *)
                              (List((gpSet, gfSet)), ansSet ++ List((hapSet, hafSet)))
                            // pSet, hapSet |= hlForm *)
                            else if ((ipSet == gpSet) || (gpSet == ipSet)) ///@@@@?@?@?@?
                              (Nil, ansSet ++ List((hapSet, hlForm :: hafSet)))
                            else (List((gpSet, gfSet ++ hafSet)),
                              ansSet ++ List((hapSet, hlForm :: hafSet)))
                          case (Nil, ansSet) => (Nil, ansSet ++ List((hapSet, hafSet)))
                          case other => throw new InvalidModelFormulaSetList()
                        }
                    }
                  }
                  floop(rlForm, cacc match {
                    case (pfSet, nacc) => nacc ++ pfSet
                  })
              }
          }

        floop(hForm :: rForm, Nil)
    }

  }
}