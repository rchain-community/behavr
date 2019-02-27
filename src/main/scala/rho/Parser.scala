package rho

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {
  def proc: Parser[Process] = proc2 ~ rep("|" ~> proc) ^^ {
    case p0 ~ Nil => p0
    case p0 ~ ps => Par(p0 :: ps)
  }

  def proc2: Parser[Process] = nil | input | lift | drop | ("{" ~> proc <~ "}")

  def nil: Parser[Process] = "nil" ^^ (_ => Zero)

  def input: Parser[Process] = ("for" ~> "(" ~> name) ~ ("<-" ~> name <~ ")") ~ ("{" ~> proc <~ "}") ^^ {
    case nsubj ~ nobj ~ cont => Input(Action(nsubj, nobj), cont)
  }

  def lift: Parser[Process] = (name <~ "!") ~ ("(" ~> proc <~ ")") ^^ { case n ~ p => Lift(n, p) }

  def drop: Parser[Process] = "*" ~> name ^^ (n => Drop(n))

  def name: Parser[Name] = "@" ~> proc2 ^^ (p => Quote(p))
}
