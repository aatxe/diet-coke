package coke

import scala.util.parsing.combinator._

import Syntax._

class Parser extends RegexParsers with PackratParsers {
  type P[A] = PackratParser[A]

  val reserved = Set(
    "fn", "let", "true", "false", "if", "then", "else", "error", "fix",
    "show", "println", "print", "catch", "inject", "random"
  )

  def notReserved: PartialFunction[String, String] = {
    case id if reserved.contains(id) == false => id
  }

  lazy val builtIns: P[String] =
    "show" | "println" | "print" | "catch" | "inject" | "random"

  lazy val idCore: P[String] =
    """([a-zA-Z]|[^\u0000-\uFFFF])([a-zA-Z0-9]|[^\u0000-\uFFFF])*""".r ^^ (x => x)

  lazy val id: P[String] = idCore ^? (notReserved, (id => s"$id is a reserved keyword."))

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

  lazy val typAtom: P[Type] =
    "()" ^^ { _ => TUnit }       |
    "num" ^^ { _ => TNum }       |
    "bool" ^^ { _ => TBool }     |
    "string" ^^ { _ => TString }

  lazy val typFun: P[Type] =
    typAtom ~ ("->" ~> typFun) ^^ { case lhs ~ rhs => TFun(lhs, TVar(), rhs) } |
    typAtom

  lazy val typ: P[Type] =
    typFun

  lazy val atom: P[Expr] =
    "()" ^^ { _ => EUnit }  |
    id ^^ { EId(_) }        |
    const ^^ { EConst(_) }  |
    builtInApp              |
    error                   |
    "(" ~> expr <~ ")"

  lazy val error: P[Expr] =
    "error" ~> string ^^ { msg => EError(msg) }

  lazy val builtIn: P[BuiltIn] =
    builtIns ^^ {
      case "show" => BShow
      case "print" => BPrint
      case "println" => BPrintln
      case "catch" => BCatch
      case "inject" => BInject
      case "random" => BRandom
    }

  lazy val builtInApp: P[Expr] =
    builtIn ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case builtIn ~ args => EBuiltIn(builtIn, args)
    }

  lazy val app: P[Expr] =
    app ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case fun ~ Nil => EApp(fun, EUnit)
      case fun ~ args => args.tail.foldLeft[Expr](EApp(fun, args.head)) {
        case (acc, arg) => EApp(acc, arg)
      }
    } |
    atom

  lazy val negNot: P[Expr] =
    "!" ~> negNot ^^ { arg => EOp1(ONot, arg) } |
    "-" ~> negNot ^^ { arg => EOp1(ONeg, arg) } |
    app

  lazy val mulDiv: P[Expr] =
    mulDiv ~ ("*" ~> negNot) ^^ { case lhs ~ rhs => EOp2(OMul, lhs, rhs) } |
    mulDiv ~ ("/" ~> negNot) ^^ { case lhs ~ rhs => EOp2(ODiv, lhs, rhs) } |
    negNot

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

  lazy val and: P[Expr] =
    and ~ ("&&" ~> numComparison) ^^ { case lhs ~ rhs => EOp2(OAnd, lhs, rhs) } |
    numComparison

  lazy val or: P[Expr] =
    or ~ ("||" ~> and) ^^ { case lhs ~ rhs => EOp2(OOr, lhs, rhs) } |
    and

  lazy val fix: P[Expr] =
    ("fix" ~> id <~ "=>") ~ expr ^^ { case id ~ expr => EFix(id, expr) }

  lazy val anonymousFun: P[Expr] =
    id ~ ("=>" ~> expr) ^^ { case id ~ body => EFun(id, body) }

  lazy val thunk: P[Expr] =
    "{" ~> rep1sep(expr, ";") <~ "}" ^^ { exprs => EBlock(exprs) }

  lazy val ifExpr: P[Expr] =
    ("if" ~> expr <~ "then") ~ expr ~ opt("else" ~> expr) ^^ {
      case pred ~ tru ~ Some(fls) => EIf(pred, tru, fls)
      case pred ~ tru ~ None => EIf(pred, tru, EUnit)
    }

  // TODO see about replacing this with an explicit precendence table.
  lazy val expr: P[Expr] =
    fix           |
    anonymousFun  |
    thunk         |
    ifExpr        |
    or

  lazy val binding: P[Statement] =
    ("let" ~> id <~ "=") ~ expr ^^ { case id ~ body => SBinding(id, body) }

  lazy val funcDecl: P[Statement] =
    ("fn" ~> id) ~ ("(" ~> repsep(id, ",") <~ ")") ~ ("=" ~> expr) ^^ {
      case id ~ Nil ~ body => SBinding(id, body)
      case id ~ args ~ body => SBinding(id, EFix(id, args.foldRight(body) {
        case (arg, acc) => EFun(arg, acc)
      }))
    }

  lazy val exprStmt: P[Statement] =
    expr ^^ { case expr => SExpr(expr) }

  lazy val stmtAtom: P[Statement] =
    binding  |
    funcDecl |
    exprStmt

  lazy val stmt: P[Statement] =
    rep1sep(stmtAtom, ";") ^^ {

      case List(stmt) => stmt
      case stmts => SBlock(stmts)
    }

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

  def parseExpr(str: String): Expr =
    parser.parseString(str, parser.expr)
}
