package coke

import Syntax._

object Pretty {
  def prettyKind(kind: Kind): String = kind match {
    case KStar => "*"
    case KRow => "e"
  }

  def prettyType(typ: Type): String = typ match {
    case TUnit => "()"
    case TNum => "num"
    case TString => "string"
    case TBool => "bool"
    case TFun(x, e, y) => s"(${prettyType(x)} -> ${prettyType(e)} ${prettyType(y)})"
    case TVar(tyvar) => tyvar.name
    case TRowEmpty => "<>"
    case TRowExtend(_, _, _) => prettyRow(typ)
  }

  def prettyRow(typ: Type): String = InferenceEngine.decomposeRow(typ) match {
    case (lpairs, Some(tyvar)) => {
      val row = lpairs.map(_._1).mkString(", ")
      s"<$row | ${tyvar.name}>"
    }
    case (lpairs, None) => {
      val row = lpairs.map(_._1).mkString(", ")
      s"<$row>"
    }
  }

  def prettyValue(value: Value): String = value match {
    case VUnit => "()"
    case VConst(c) => prettyConst(c)
    case VFix(id, expr) => s"fix $id => ${prettyExpr(expr)}"
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
    case EFix(id, expr) => s"(fix $id => ${prettyExpr(expr)})"
    case EFun(id, body) => s"($id => ${prettyExpr(body)})"
    case EApp(fun, arg) => s"${prettyExpr(fun)}(${prettyExpr(arg)})"
    case EBuiltIn(builtIn, args) => s"${prettyBuiltIn(builtIn)}(" + args.tail.foldLeft(prettyExpr(args.head))((acc, expr) => s"$acc, ${prettyExpr(expr)}") + ")"
    case EIf(pred, tru, fls) => s"if ${prettyExpr(pred)} then ${prettyExpr(tru)} else ${prettyExpr(fls)}"
    case EBlock(exprs) => "{ " + exprs.tail.foldLeft(prettyExpr(exprs.head))((acc, expr) => s"$acc; ${prettyExpr(expr)}") + " }"
  }

  def prettyBuiltIn(builtIn: BuiltIn): String = builtIn match {
    case BShow => "show"
    case BPrint => "print"
    case BPrintln => "println"
    case BCatch => "catch"
    case BInject => "inject"
    case BRandom => "random"
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
