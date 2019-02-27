package slmc

import java.io.{PrintStream, ByteArrayOutputStream}

import org.scalatest.FunSuite

class PiastnodeTest extends FunSuite {

  import Piastnode.{astFN, print_ast}

  // example from section 3 Syntax of Processes
  // of http://ctp.di.fct.unl.pt/SLMC/manual.pdf
  val eg1 = Par(New(List("secret"), Act(Output("hand", List("secret")), Void)), Act(Input("erase", List("x")), Void))

  test("example 1 has no free names") {
    assert(astFN(eg1, Set()) == Set())
  }

  test("print example 1 from the manual") {
    val buf = new ByteArrayOutputStream
    val out = new PrintStream(buf, true, "UTF-8")
    print_ast(eg1)(out)
    assert(buf.toString("UTF-8") == "(new secret in hand!(secret).0 | erase?(x).0)")
  }
}
