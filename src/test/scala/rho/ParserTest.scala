package rho

import org.scalatest.{Assertion, FunSuite}


class ParserTest extends FunSuite {

  object P extends Parser {
    def run(s: String, p: Process): Assertion = parse(phrase(proc), s) match {
      case Success(matched, _) => assert(matched == p)
      case Failure(msg, _) => assert(s"FAILURE: $msg" == "")
      case Error(msg, _) => assert(s"ERROR: $msg" == "")
    }
  }

  val cases = List(
    ("nil", Zero),
    ("for(@nil <- @nil) { nil }", Input(Action(Quote(Zero), Quote(Zero)), Zero)),
    ("@nil!(nil)", Lift(Quote(Zero), Zero)),
    ("*@nil", Drop(Quote(Zero))),
    ("{ @nil!(nil) | nil }", Par(List(Lift(Quote(Zero),Zero), Zero))),
  )

  for ((s, p) <- cases) {
    test(s) {
      P.run(s, p)
    }
  }

  for {
    (sp, p) <- cases
    (sq, q) <- cases
  } {
    test(s"parse $sp | $sq") {
      P.run(s"$sp | $sq", Par(List(p, q)))
    }
  }
}
