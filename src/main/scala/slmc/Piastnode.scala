package slmc

//** Module that handles the abstract syntactic tree representation for processes ***)

class Ill_formed_ast extends Exception

//** Action type ***)
sealed trait test

case object Equals extends test

case object Differs extends test

//** Action type ***)
sealed trait action

case class Input(n: String, ln: List[String]) extends action

case class Output(n: String, ln: List[String]) extends action

case object Tau extends action

//** Abstract syntactic tree for processes type ***)
sealed trait Piastnode

case object Void extends Piastnode

case class Par(ast1: Piastnode, ast2: Piastnode) extends Piastnode

case class Sum(ast1: Piastnode, ast2: Piastnode) extends Piastnode

case class New(l: List[String], ast: Piastnode) extends Piastnode

case class Act(a: action, astc: Piastnode) extends Piastnode

case class Test(id1: String, id2: String, ast: Piastnode, t: test) extends Piastnode

case class Var(x: String, aL: List[String]) extends Piastnode

//** Declaration environment type ***)
sealed trait dec

case class Pidec(arg1: List[String], lls: List[List[String]], p: List[Piastnode]) extends dec // ref

//** Process environment type ***)
object Piastnode {
  import java.io.PrintStream

  type ast_env = Map[String, (List[String], Piastnode)] // ref

  //**)

  //** Computes the set of free names of a process ast ***)
  def astFN(ast: Piastnode, ht: Set[String]): Set[String] = ast match {
    case Void => Set()
    case Par(ast1, ast2) =>
      astFN(ast1, ht) ++ astFN(ast2, ht)
    case Sum(ast1, ast2) =>
      astFN(ast1, ht) ++ astFN(ast2, ht)
    case New(l, astc) =>
      astFN(astc, ht ++ l.toSet).diff(l.toSet)
    case Act(a, astc) => a match {
      case Output(n, ln) => astFN(astc, ht ++ (n :: ln).toSet)
      case Input(n, ln) =>
        astFN(astc, ht ++ Set(n) ++ ln.toSet).diff(ln.toSet)
      case Tau => astFN(astc, ht)
    }
    case Test(id1, id2, astc, _) =>
      astFN(astc, ht ++ Set(id1, id2))
    case Var(_, aL) => aL.toSet
  }

  //**)

  // Auxiliar function to print_ast *)

  def print_idlist[L](lst: L, flag: Boolean)(implicit o: PrintStream):Unit = lst match {
    case Nil => if (flag) print(" ")
    case h :: Nil =>
      o.print(h)
      if (flag)
        o.print(" ")
    case h :: t =>
      o.print(h)
      o.print(",")
      print_idlist(t, flag)
  }

  //** Prints a process ast to stdout ***)
  def print_ast(ast: Piastnode)(implicit o: PrintStream):Unit = ast match {
    case Void =>
      o.print("0")
    case Par(ast1, ast2) =>
      o.print("(")
      print_ast(ast1)
      o.print(" | ")
      print_ast(ast2)
      o.print(")")
    case Sum(ast1, ast2) =>
      o.print("{")
      print_ast(ast1)
      o.print(" + ")
      print_ast(ast2)
      o.print("}")
    case New(lst, ast1) =>
      o.print("new ")
      print_idlist(lst, flag = true)
      o.print("in ")
      print_ast(ast1)
    case Act(a, ast1) =>
      a match {
        case Input(n1, n2) =>
          o.print(n1)
          o.print("?(")
          print_idlist(n2, flag = false)
          o.print(").")
          print_ast(ast1)
        case Output(n1, n2) =>
          o.print(n1)
          o.print("!(")
          print_idlist(n2, flag = false)
          o.print(").")
          print_ast(ast1)
        case Tau =>
          o.print("tau.")
          print_ast(ast1)
      }
    case Test(id1, id2, ast1, typ) =>
      o.print("[")
      o.print(id1)
      if (typ == Equals)
        o.print("=")
      else
        o.print("!=")
      o.print(id2)
      o.print("].")
      print_ast(ast1)
    case Var(x, aL) =>
      o.print(x)
      if (aL.nonEmpty) {
        o.print("(")
        print_idlist(aL, flag = false)
        o.print(")")
      }
  }
}
//**)
