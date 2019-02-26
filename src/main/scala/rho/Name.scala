package rho

sealed trait Name

case class Quote(p: Process) extends Name

object Name {
  def equivalent(n1: Name, n2: Name): Boolean = (n1, n2) match {
    case (Quote(Drop(n11)), _) => equivalent(n11, n2)
    case (_, Quote(Drop(n21))) => equivalent(n1, n21)
    case (Quote(p1), Quote(p2)) => Process.equivalent(p1, p2)
  }
}