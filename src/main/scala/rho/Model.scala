package rho

class Model {

}

object Model {
  import Formula.logicalSubstitution
  import Process.syntacticSubstitution

  def satisfies(proc: Process, form: Formula): Boolean = form match {
    case True => true
    // case  ZeroF => structurallyEquivalent proc (zero)
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
          case Naming (Quote(q)) => Name.equivalent( Quote (p), Quote (q))
        }
    }

  def parMix(process: Process, processes: List[Process], formula: Formula, formulas: List[Formula]) = false // TODO
}