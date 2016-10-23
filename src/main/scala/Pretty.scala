package coke

import Syntax._

object Pretty {
  def prettyValue(value: Value): String = value match {
    case VUnit => "()"
    case VConst(c) => prettyConst(c)
    case VClosure(id, body, _) => s"$id => ${prettyExpr(body)}"
    case VThunk(body, _) => "{ " + body.tail.foldLeft(prettyExpr(body.head))((acc, expr) => s"$acc; ${prettyExpr(expr)}") + " }"
    case VExpr(e) => prettyExpr(e)
  }

  def prettyConst(const: Const): String = const match {
    case CNum(n) => n.toString
    case CBool(b) => b.toString
    case CString(s) => '"' + s +'"'
  }

  def prettyExpr(expr: Expr): String = expr match {
    case EUnit => "()"
    case EError(msg) => "error \"" + msg + "\""
    case EId(id) => id
    case EConst(c) => prettyConst(c)
    case EOp1(op, expr) => s"(${prettyOp1(op)}${prettyExpr(expr)})"
    case EOp2(op, lhs, rhs) => s"(${prettyExpr(lhs)} ${prettyOp2(op)} ${prettyExpr(rhs)})"
    case EFun(id, body) => s"($id => ${prettyExpr(body)})"
    case EApp(fun, arg) => s"$fun($arg)"
    case EBuiltIn(builtIn, args) => s"${prettyBuiltIn(builtIn)}(" + args.tail.foldLeft(prettyExpr(args.head))((acc, expr) => s"$acc, ${prettyExpr(expr)}") + ")"
    case EIf(pred, tru, fls) => s"if $pred then $tru else $fls"
    case EBlock(exprs) => "{ " + exprs.tail.foldLeft(prettyExpr(exprs.head))((acc, expr) => s"$acc; ${prettyExpr(expr)}") + " }"
  }

  def prettyBuiltIn(builtIn: BuiltIn): String = builtIn match {
    case BShow => "show"
    case BPrint => "print"
    case BPrintln => "println"
    case BCatch => "catch"
    case BInject => "inject"
  }

  def prettyOp1(op1: Op1): String = op1 match {
    case ONot => "!"
    case ONeg => "-"
  }

  def prettyOp2(op2: Op2): String = op2 match {
    case OAdd => "+"
    case OSub => "-"
    case OMul => "*"
    case ODiv => "/"
    case OMod => "%"
    case OLt => "<"
    case OLte => "<="
    case OGt => ">"
    case OGte => ">="
    case OEq => "=="
    case ONEq => "/="
    case OConcat => "^"
    case OAnd => "&&"
    case OOr => "||"
  }
}
