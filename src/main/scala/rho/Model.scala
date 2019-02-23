package rho

class Model {

}

object Model {
  def satisfies(proc: Process, form: Formula): Boolean = form match {
    case True => true
    // case  ZeroF => structurallyEquivalent proc (zero)
    case Negation(nForm) => !satisfies(proc, nForm)
    case Conjunction(Nil) => false
    case Conjunction(hForm :: rForm) =>
      (hForm :: rForm).forall { satisfies(proc, _) }
    case Mixture(Nil) => Process.equivalent(proc, Zero)
    case Mixture(hForm :: rForm) => proc match {
      case Par(Nil) => (hForm :: rForm).forall {
        _ == ZeroF
      }
      case Par(hProc :: rProc) => parMix(hProc, rProc, hForm, rForm)
      case pOther => false
    }
    case Descent(m) => // LGM: should we require exact match in this case and let the user specify the true?
      proc match {
        case Drop(n) => Name.equivalent(m, n)
        case Par(hProc :: rProc) =>
          if (!satisfies(hProc, Descent(m))) satisfies(Par(rProc), Descent(m))
          else true
        case _ => false
      }
    // more cases...
  }

  def parMix(process: Process, processes: List[Process], formula: Formula, formulas: List[Formula]) = false // TODO
}