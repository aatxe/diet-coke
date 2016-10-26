package coke

import Syntax._

object Subst {
  def empty(): Subst = Subst(Map())
  def singleton(key: Type, value: Type): Subst = Subst(Map(key -> value))
}

case class Subst private(map: Map[Type, Type]) {
  def apply(typ: Type): Type = typ match {
    case TUnit | TNum | TString | TBool => typ
    case meta@TMetavar(_) => map.getOrElse(meta, typ)
    case TFun(lhs, rhs) => TFun(this(lhs), this(rhs))
  }

  def apply(subst: Subst): Subst = Subst(subst.map.mapValues(typ => this(typ)))

  def compose(subst: Subst): Subst = Subst(subst(this).map ++ this(subst).map)

  // Gets the type to update.
  def updateType(typ: Type): Type = map.getOrElse(typ, typ)

  // Updates the types in the expression tree based on this substitution.
  def apply(expr: Expr): Expr = expr match {
    case EUnit | EError(_) | EId(_) | EConst(_) => expr.setType(updateType(expr.typ))
    case EOp1(op1, body) => EOp1(op1, this(body)).setType(updateType(expr.typ))
    case EOp2(op2, lhs, rhs) => EOp2(op2, this(lhs), this(rhs)).setType(updateType(expr.typ))
    case EFun(id, body) => EFun(id, this(body)).setType(updateType(expr.typ))
    case EApp(fun, arg) => EApp(this(fun), this(arg)).setType(updateType(expr.typ))
    case EBuiltIn(builtIn, args) => EBuiltIn(builtIn, args.map(this(_))).setType(updateType(expr.typ))
    case EIf(pred, tru, fls) => EIf(this(pred), this(tru), this(fls)).setType(updateType(expr.typ))
    case EBlock(exprs) => EBlock(exprs.map(this(_))).setType(updateType(expr.typ))
  }

  // Updates the types in the statement tree based on this substitution.
  def apply(stmt: Statement): Statement = stmt match {
    case SBinding(id, body) => SBinding(id, this(body))
    case SExpr(expr) => SExpr(this(expr))
    case SBlock(stmts) => SBlock(stmts.map(this(_)))
  }
}
