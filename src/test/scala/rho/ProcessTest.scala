package rho

import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, PropSpec}

object ArbProc {

  import org.scalacheck.Gen._
  import org.scalacheck._

  def log[T](it: T): T = {
    println("@@@@@gen:" + it.toString)
    it
  }
  def genProc: Gen[Process] = oneOf(genZero, genInput, genLift, genDrop, genPar)
  val genZero = const(log(Zero))
  val genName: Gen[Name] = for { p <- genProc } yield log(Quote(p))
  val genInput = for {
    subj <- genName
    obj <- genName
    cont <- genProc
  } yield log(Input(Action(subj, obj), cont))
  val genLift = for {
    obj <- genName
    cont <- genProc
  } yield log(Lift(obj, cont))
  val genDrop = for { n <- genName } yield log(Drop(n))
  val genPar = for { ps <- Gen.containerOf[List, Process](genProc) } yield log(Par(ps))

  implicit lazy val arbProc: Arbitrary[Process] = Arbitrary(genProc)
}

class ProcessTest extends FunSuite with PropertyChecks {
  import Process._

  val n1 = Quote(Par(List(Zero, Zero)))
  val p0: Process = zero
  val p1 = Lift(n1, Zero)
  val p2 = Lift(Quote(Zero),Zero)

  test("Zero is equivalent to itself") {
    assert(equivalent(Zero, Zero))
  }

  test("parFlattens") {
    val actual = par(Par(List(p0, p1)), Par(List(p1, p0)))
    val expected = Par(List(p0, p1, p1, p0))
    assert(actual === expected)
  }

  test("simple normalization") {
    assert(p1 != Lift(Quote(Zero),Zero))
    assert(normalize(p1) == Lift(Quote(Zero),Zero))
  }

  test("normalize removes dups") {
    assert(normalize(Par(List(p2, p2))) == p2)
  }

}


class ProcessSpec extends PropSpec with PropertyChecks {
  import Process._
  import ArbProc.arbProc

  property("normalization is idempotent") {
    forAll ("pnorm") { p: Process =>
      normalize(p) === normalize(normalize(p))
    }
  }

  property("equivalence by normalization") {
    forAll { (p1: Process, p2: Process) =>
      equivalent(p1, p2) === (normalize(p1) == normalize(p2))
    }
  }
}