package coke

import Syntax._

object InferenceEngine {

  def freeTypeVars(scheme: Scheme): Set[TyVar] = scheme.typ.freeTypeVars diff scheme.tyvars.toSet

  def freeTypeVars(env: TEnv): Set[TyVar] = env.values.foldRight[Set[TyVar]](Set()) {
    case (scheme, acc) => acc union freeTypeVars(scheme)
  }

  def generalize(typ: Type)(implicit env: TEnv): Scheme = Scheme(
    typ.freeTypeVars.diff(freeTypeVars(env)).toSeq, typ
  )

  def instantiate(scheme: Scheme): Type = Subst(scheme.tyvars.zip(scheme.tyvars.map({
    case TyVar(name, kind, lacks) => TVar(name.takeWhile(c => !c.isDigit), kind, lacks)
  })).toMap)(scheme.typ)

  def unify(t1: Type, t2: Type): Subst = (t1, t2) match {
    case (TFun(l1, e1, r1), TFun(l2, e2, r2)) => {
      val s1 = unify(l1, l2)
      val s2 = unify(s1(e1), s1(e2))
      val s3 = s2 compose s1
      val s4 = unify(s3(r1), s3(r2))
      s4 compose s3
    }
    case (TRowExtend(label1, typ1, tail1), row2@TRowExtend(_, _, _)) => {
      val (typ2, tail2, s1) = rewriteRow(row2, label1)
      decomposeRow(tail1) match {
        case (_, Some(tv)) if s1 contains tv => throw Errors.RecursiveRowFound
        case _ => {
          val s2 = unify(s1(typ1), s1(typ2))
          val s3 = s2 compose s1
          val s4 = unify(s3(tail1), s3(tail2))
          s4 compose s3
        }
      }
    }
    case (TVar(tv1), TVar(tv2)) => unionConstraints(tv1, tv2)
    case (TVar(tv), typ) => varBind(tv, typ)
    case (typ, TVar(tv)) => varBind(tv, typ)
    case (TUnit, TUnit) | (TNum, TNum) | (TBool, TBool) |
         (TString, TString) | (TRowEmpty, TRowEmpty) => Subst.empty
    case _ => throw Errors.UnificationFailed(t1, t2)
  }

  def unionConstraints(tv1: TyVar, tv2: TyVar): Subst = (tv1, tv2) match {
    case _ if tv1 == tv2 => Subst.empty
    case (TyVar(_, KStar, _), TyVar(_, KStar, _)) => Subst.singleton(tv1, TVar(tv2))
    case (TyVar(_, KRow, c1), TyVar(_, KRow, c2)) => {
      val fresh = TVar(c1 union c2)
      Subst.singleton(tv1, fresh) compose Subst.singleton(tv2, fresh)
    }
    case (TyVar(_, k1, _), TyVar(_, k2, _)) => throw Errors.KindMismatch(k1, k2)
  }

  def varBind(tv: TyVar, typ: Type): Subst = tv.kind match {
    case _ if typ.freeTypeVars contains tv => throw Errors.OccursCheckFailed(TVar(tv), typ)
    case KStar => Subst.singleton(tv, typ)
    case KRow => varBindRow(tv, typ)
  }

  def varBindRow(tv: TyVar, typ: Type): Subst = {
    val (lpairs, tvPrime) = decomposeRow(typ)
    val labels = lpairs.map(_._1).toSet

    val s1 = Subst.singleton(tv, typ)
    (tv.lacks.intersect(labels).toSeq, tvPrime) match {
      case (Seq(), None) => s1
      case (Seq(), Some(r1)) => {
        val r2 = TVar(labels union r1.lacks)
        val s2 = Subst.singleton(r1, r2)
        s1 compose s2
      }
      case (labels, _) => throw Errors.RowContainedIllegalLabels(typ, labels.toSet)
    }
  }

  def decomposeRow(typ: Type): (Seq[(String, Type)], Option[TyVar]) = typ match {
    case TVar(v) => (Seq(), Some(v))
    case TRowEmpty => (Seq(), None)
    case TRowExtend(label, typ, tail) => {
      val (labels, tv) = decomposeRow(tail)
      ((label -> typ) +: labels, tv)
    }
    case _ => throw Errors.Unreachable
  }

  def rewriteRow(rowTyp: Type, newLabel: String): (Type, Type, Subst) = rowTyp match {
    case TRowEmpty => throw Errors.RowRewriteFailed(rowTyp, newLabel)
    case TRowExtend(label, typ, tail) if label == newLabel => (typ, tail, Subst.empty)
    case TRowExtend(label, typ, tail) => tail match {
      case TVar(tv1) => {
        val freshEffect = TVar(Set(newLabel))
        val fresh = TVar("a")
        val s = varBindRow(tv1, TRowExtend(newLabel, fresh, freshEffect))
        (fresh, s(TRowExtend(label, typ, freshEffect)), s)
      }
      case _ => {
        val (typPrime, tailPrime, s) = rewriteRow(tail, newLabel)
        (typPrime, TRowExtend(label, typ, tailPrime), s)
      }
    }
  }

  def infer(expr: Expr)(implicit env: TEnv): (Subst, Type, Effect) = expr match {
    case EUnit => (Subst.empty, TUnit, TVar())
    case EConst(const) => (Subst.empty, const.typ, TVar())
    case EError(_) => (Subst.empty, TVar("a"), TRowExtend("exn", TUnit, TVar()))
    case EId(id) => env.get(id) match {
      case None => throw Errors.UnboundIdentifier(id)
      case Some(scheme) => (Subst.empty, instantiate(scheme), TVar())
    }
    case EOp1(op1, expr) => {
      val (s1, t, e) = infer(expr)
      val tv = TVar("a")
      val s2 = unify(TFun(s1(t), TVar(), tv), op1.typ)
      (s2, s2(tv), e)
    }
    case EOp2(op2, lhs, rhs) => {
      val (s1, t1, e1) = infer(lhs)
      val (s2, t2, e2) = infer(rhs)
      val tv = TVar("a")
      val s3 = unify(TFun(s1(t1), TVar(), TFun(s2(t2), TVar(), tv)), op2.typ)
      val s4 = unify(e1, e2)
      (s3, s3(tv), s4(e2))
    }
    case EFix(id, expr) => {
      val (_, t, e) = infer(expr)
      val tv1 = TVar("a")
      val tv2 = TVar("a")
      val s = unify(TFun(tv1, e, tv2), t)
      (s, s(t), s(e))
    }
    case EFun(id, body) => {
      val tv = TVar("a")
      val (s, t, e) = infer(body)(env + (id -> Scheme(Seq(), tv)))
      (s, s(TFun(tv, e, t)), TVar())
    }
    case EApp(fun, arg) => {
      val (s1, t1, e1) = infer(fun)
      val (s2, t2, e2) = infer(arg)
      val tv = TVar("a")
      val e3 = TVar()
      val s3 = unify(s1(t1), TFun(s2(t2), e3, tv))
      val s4 = unify(e2, e3)
      val s5 = s4 compose s3
      (s5, s5(tv), s5(e3))
    }
    case EBuiltIn(builtIn, args) => builtIn match {
      // Arity 0
      case BRandom => args match {
        case Seq() => {
          val tv = TVar("a")
          val e = TVar()
          val s = unify(builtIn.typ, TFun(TUnit, e, tv))
          (s, s(tv), s(e))
        }
        case _ => throw Errors.ArityMismatch(builtIn, args.length, 0)
      }

      // Arity 1
      case BShow | BPrint | BPrintln | BInject => args match {
        case Seq(arg) => {
          val (s1, t1, e1) = infer(arg)
          val tv = TVar("a")
          val e2 = TVar()
          val s2 = unify(builtIn.typ, TFun(s1(t1), s1(e1), tv))
          val s3 = unify(s2(e1), s2(e2))
          val s4 = s3 compose s2
          (s4, s4(tv), s4(e2))
        }
        case _ => throw Errors.ArityMismatch(builtIn, args.length, 1)
      }

      // Arity 2
      case BCatch => args match {
        case Seq(lhs, rhs) => {
          val (s1, t1, e1) = infer(lhs)
          val (s2, t2, e2) = infer(rhs)
          val tv = TVar("a")
          val e3 = TVar()
          val s3 = unify(builtIn.typ, TFun(TFun(TUnit, s1(e1), s1(t1)), TVar(), TFun(TFun(TString, s2(e2), s2(t2)), e3, tv)))
          val s4 = s3 compose s2 compose s1
          val s5 = unify(s4(e1), s4(e3))
          val s6 = unify(s4(e2), s4(e3))
          val s7 = s6 compose s5
          (s7, s7(tv), s6(e3))
        }
        case _ => throw Errors.ArityMismatch(builtIn, args.length, 2)
      }
    }
    case EIf(pred, tru, fls) => {
      val (s1, t1, e1) = infer(pred)
      val (s2, t2, e2) = infer(tru)
      val (s3, t3, e3) = infer(fls)
      val s4 = unify(s1(t1), TBool)
      val s5 = unify(s2(t2), s3(t3))
      val s6 = s5 compose s4
      (s6, s6(t2), e2)
    }
    case EBlock(exprs) => exprs.foldLeft[(Subst, Type, Effect)]((Subst.empty, TUnit, TVar())) {
      case ((_, _, e1), expr) => infer(expr) match {
        case (s1, t, e2) => {
          val tv = TVar("a")
          val s2 = unify(s1(t), tv)
          val s3 = unify(e1, e2)
          (s2, s2(tv), s3(e2))
        }
      }
    } match {
      case (s, t, e) => (s, TFun(TUnit, e, t), TVar())
    }
  }
}
