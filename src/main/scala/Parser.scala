package coke

import scala.util.parsing.combinator._

import Syntax._

class Parser extends RegexParsers with PackratParsers {
  type P[A] = PackratParser[A]

  lazy val reserved: P[String] =
    "fn" | "let" | "true" | "false"

  lazy val id: P[String] =
    guard(not(reserved)) ~> """([a-zA-Z]|[^\u0000-\uFFFF])([a-zA-Z0-9]|[^\u0000-\uFFFF])*""".r

  lazy val string: P[String] =
    "\"" ~> """[^"]""".r <~ "\""

  lazy val boolean: P[Boolean] =
    "true" ^^ { _ => true }
    "false" ^^ { _ => false }

  lazy val integer: P[Int] =
    """[0-9]+""".r ^^ { n => Integer.parseInt(n) }

  lazy val const: P[Const] =
    integer ^^ { CNum(_) } |
    boolean ^^ { CBool(_) } |
    string ^^ { CString(_) }

  lazy val atom: P[Expr] =
    id ^^ { EId(_) } |
    const ^^ { EConst(_) } |
    "(" ~> expr <~ ")"

  lazy val expr: P[Expr] =
    atom

  lazy val binding: P[Statement] =
    ("let" ~> id <~ "=") ~ expr ^^ { case id ~ body => SBinding(id, body) }

  lazy val stmt: P[Statement] =
    binding

  def parseString[A](str: String, parser: P[A]): A =
    parseAll(parser, str) match {
      case Success(r, _) => r
      case err => throw Errors.ParsingFailed(s"$err")
    }
}

object Parser {
  private val parser = new Parser()

  def parse(str: String): Statement =
    parser.parseString(str, parser.stmt)
}
