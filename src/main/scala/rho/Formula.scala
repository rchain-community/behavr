package rho

/**
  * ref
  *
  * Namespace Logic - A Logic for a Reflective Higher-Order Calculus.
  * Meredith L., Radestock M 2005
  * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.95.9601
  *
  * transcribed from https://github.com/leithaus/rhocaml/blob/master/logic.ml
  * da884fb  on Jun 19, 2016 Meredith Gregory
  */
sealed trait Formula

case object True extends Formula // aka verity
case object ZeroF extends Formula // aka nullity
case class Negation(f: Formula) extends Formula

case class Conjunction(fs: List[Formula]) extends Formula

case class Mixture(fs: List[Formula]) extends Formula // aka separation?
case class Descent(n: Name) extends Formula

case class Elevation(i: Indicator, f: Formula) extends Formula

case class Activity(c: Condition, f: Formula) extends Formula

// That's it from logic.ml, but the paper also has: fix point, quantification.

sealed trait Indicator

case class Quotation(f: Formula) extends Indicator

case class Naming(n: Name) extends Indicator

case class Condition(i: Indicator, n: Name)
