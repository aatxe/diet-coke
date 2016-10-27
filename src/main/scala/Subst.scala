package coke

import Syntax._

object Subst {
  def empty(): Subst = Subst(Map())
  def singleton(key: TyVar, value: Type): Subst = Subst(Map(key -> value))
}

case class Subst(map: Map[TyVar, Type]) {
  def apply(typ: Type): Type = typ match {
    case TUnit | TNum | TString | TBool | TRowEmpty => typ
    case TVar(tyvar) => map.getOrElse(tyvar, typ)
    case TFun(lhs, effect, rhs) => TFun(this(lhs), this(effect), this(rhs))
    case TRowExtend(l, lhs, rhs) => TRowExtend(l, this(lhs), this(rhs))
  }

  def apply(subst: Subst): Subst = Subst(subst.map.mapValues(typ => this(typ)))

  def apply(scheme: Scheme): Scheme = scheme match {
    case Scheme(tyvars, typ) => Scheme(tyvars, Subst(map -- tyvars.toSet)(typ))
  }

  def apply(env: TEnv): TEnv = env.mapValues(this(_))

  def compose(subst: Subst): Subst = Subst(subst(this).map ++ this(subst).map)

  def contains(key: TyVar): Boolean = map.contains(key)
}
