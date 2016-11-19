package coke

import Syntax._

object Pretty {
  def prettyKind(kind: Kind): String = kind match {
    case KStar => "*"
    case KRow => "e"
  }

  def getTypeVarSimplification(typ: Type): Map[TyVar, String] = {
    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a.flatMap { x => b.map { y => f(x, y) } }

    def sequence[A](a: Stream[Stream[A]]): Stream[Stream[A]] =
      a.foldRight[Stream[Stream[A]]](Stream(Stream()))((x, y) => map2(x, y)(_ #:: _))

    def replicate[A](n: Int, s: Stream[A]): Stream[Stream[A]] =
      sequence(Stream.fill(n)(s))

    val letterRng = 'a' to 'z'
    var letters = Stream.from(1).flatMap(n => replicate(n, letterRng.toStream))

    typ.freeTypeVars.map {
      tv => {
        val res = tv -> letters.head.mkString
        letters = letters.tail
        res
      }
    }.toMap
  }

  def prettyType(typ: Type): String = prettyType(typ, getTypeVarSimplification(typ))

  def prettyType(typ: Type, simp: Map[TyVar, String]): String = typ match {
    case TUnit => "()"
    case TNum => "num"
    case TString => "string"
    case TBool => "bool"
    case TFun(x, e, y) => s"(${prettyType(x, simp)} -> ${prettyType(e, simp)} ${prettyType(y, simp)})"
    case TVar(tyvar) => simp(tyvar)
    case TRowEmpty => "<>"
    case TRowExtend(_, _, _) => prettyRow(typ, simp)
  }

  def prettyRow(typ: Type, simp: Map[TyVar, String]): String = InferenceEngine.decomposeRow(typ) match {
    case (lpairs, Some(tyvar)) => {
      val row = lpairs.map(_._1).mkString(", ")
      s"<$row | ${simp(tyvar)}>"
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
