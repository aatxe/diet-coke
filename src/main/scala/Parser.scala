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
    "\"" ~> """[^"]*""".r <~ "\""

  lazy val boolean: P[Boolean] =
    "true" ^^ { _ => true }   |
    "false" ^^ { _ => false }

  lazy val integer: P[Int] =
    """[0-9]+""".r ^^ { n => Integer.parseInt(n) }

  lazy val const: P[Const] =
    integer ^^ { CNum(_) }   |
    boolean ^^ { CBool(_) }  |
    string ^^ { CString(_) }

  lazy val atom: P[Expr] =
    id ^^ { EId(_) }       |
    const ^^ { EConst(_) } |
    "(" ~> expr <~ ")"

  lazy val app: P[Expr] =
    app ~ ("(" ~> rep1sep(expr, ",") <~ ")") ^^ {
      case fun ~ args => args.tail.foldLeft[Expr](EApp(fun, args.head)) {
        case (acc, arg) => EApp(acc, arg)
      }
    } |
    atom

  lazy val mulDiv: P[Expr] =
    mulDiv ~ ("*" ~> app) ^^ { case lhs ~ rhs => EOp2(OMul, lhs, rhs) } |
    mulDiv ~ ("/" ~> app) ^^ { case lhs ~ rhs => EOp2(ODiv, lhs, rhs) } |
    app

  lazy val mod: P[Expr] =
    mod ~ ("%" ~> mulDiv) ^^ { case lhs ~ rhs => EOp2(OMod, lhs, rhs) } |
    mulDiv

  lazy val addSub: P[Expr] =
    addSub ~ ("^" ~> mod) ^^ { case lhs ~ rhs => EOp2(OConcat, lhs, rhs) } |
    addSub ~ ("+" ~> mod) ^^ { case lhs ~ rhs => EOp2(OAdd, lhs, rhs) }    |
    addSub ~ ("-" ~> mod) ^^ { case lhs ~ rhs => EOp2(OSub, lhs, rhs) }    |
    mod

  lazy val numComparison: P[Expr] =
    numComparison ~ ("==" ~> addSub) ^^ { case lhs ~ rhs => EOp2(OEq, lhs, rhs) }  |
    numComparison ~ ("/=" ~> addSub) ^^ { case lhs ~ rhs => EOp2(ONEq, lhs, rhs) } |
    numComparison ~ (">" ~> addSub) ^^ { case lhs ~ rhs => EOp2(OGt, lhs, rhs) }   |
    numComparison ~ ("<" ~> addSub) ^^ { case lhs ~ rhs => EOp2(OLt, lhs, rhs) }   |
    numComparison ~ (">=" ~> addSub) ^^ { case lhs ~ rhs => EOp2(OGte, lhs, rhs) } |
    numComparison ~ ("<=" ~> addSub) ^^ { case lhs ~ rhs => EOp2(OLte, lhs, rhs) } |
    addSub

  lazy val anonymousFun: P[Expr] =
    id ~ ("=>" ~> expr) ^^ { case id ~ body => EFun(id, body) }

  lazy val ifExpr: P[Expr] =
    ("if" ~> expr <~ "then") ~ expr ~ opt("else" ~> expr) ^^ {
      case pred ~ tru ~ Some(fls) => EIf(pred, tru, fls)
      case pred ~ tru ~ None => EIf(pred, tru, EUnit)
    }

  // TODO see about replacing this with an explicit precendence table.
  lazy val expr: P[Expr] =
    anonymousFun  |
    ifExpr        |
    numComparison

  lazy val binding: P[Statement] =
    ("let" ~> id <~ "=") ~ expr ^^ { case id ~ body => SBinding(id, body) }

  lazy val exprStmt: P[Statement] =
    expr ^^ { case expr => SExpr(expr) }

  lazy val stmt: P[Statement] =
    binding  |
    exprStmt

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
