package coke

import Syntax._

object InferenceEngine {
  type Constraint = (Type, Type)

  def getArgType(op: Op1): Type = op match {
    case ONot => TBool
    case ONeg => TNum
  }

  def getArgType(op: Op2): Type = op match {
    case OAdd | OSub | OMul | ODiv | OMod => TNum
    case OLt | OLte | OGt | OGte | OEq | ONEq => TNum
    case OConcat => TString
    case OAnd | OOr => TBool
  }

  def generateConstraints(stmt: Statement, env: TEnv): (Seq[Constraint], TEnv) = stmt match {
    case SBinding(id, body) => {
      val envPrime = env + (id -> body.typ)
      (generateConstraints(body, envPrime), envPrime)
    }
    case SExpr(expr) => (generateConstraints(expr, env), env)
    case SBlock(stmts) => stmts.foldLeft[(Seq[Constraint], TEnv)]((Seq(), Map())) {
      case ((acc, env), stmt) => {
        val (accPrime, envPrime) = generateConstraints(stmt, env)
        (accPrime ++ acc, envPrime)
      }
    }
  }

  def generateConstraints(expr: Expr, initEnv: TEnv): Seq[Constraint] = {
    var res: Seq[Constraint] = Seq()

    def generate(expr: Expr)(implicit env: TEnv): Type = expr match {
      case EUnit | EError(_) => TUnit

      case EId(id) => {
        res = (expr.typ -> env(id)) +: res
        env(id)
      }

      case EConst(c) => c.typ

      case EOp1(op1, expr) => {
        res = Seq(
          expr.typ -> getArgType(op1),
          expr.typ -> generate(expr)
        ) ++ res
        op1.typ
      }

      case EOp2(op2, lhs, rhs) => {
        res = Seq(
          lhs.typ -> getArgType(op2),
          rhs.typ -> getArgType(op2),
          lhs.typ -> generate(lhs),
          rhs.typ -> generate(rhs)
        ) ++ res
        op2.typ
      }

      case EFun(id, body) => expr.typ match {
        case TFun(argT, resT) => {
          val resGenT = generate(body)(env + (id -> argT))
          res = (resT -> resGenT) +: res
          TFun(argT, resGenT)
        }
        case _ => throw Errors.Unreachable
      }

      case EApp(fun, arg) => {
        val funT = generate(fun)
        val argT = generate(arg)
        res = (funT -> TFun(argT, expr.typ)) +: res
        expr.typ
      }

      case EBuiltIn(builtIn, args) => {
        builtIn match {
          case BShow => args match {
            case Seq(arg) => {
              // show is polymorphic over all types, and thus generates no constraints.
              // This saves us from having to actually implement polymorphism.
              val argT = generate(arg)
              TString
            }
            case _ => throw Errors.ArityMismatch(BShow, args.length, 1)
          }

          case BPrint | BPrintln => args match {
            case Seq(arg) => {
              val argT = generate(arg)
              res = (argT -> TString) +: res
              TUnit
            }
            case _ => throw Errors.ArityMismatch(builtIn, args.length, 1)
          }

          case BCatch => args match {
            case Seq(thunk, handler) => {
              val thunkT = generate(thunk)
              val handlerT = generate(handler)
              res = Seq(
                thunk.typ -> thunkT,
                handler.typ -> handlerT,
                handler.typ -> TFun(TString, thunk.typ),
                expr.typ -> thunk.typ
              ) ++ res
              expr.typ
            }
            case _ => throw Errors.ArityMismatch(BCatch, args.length, 2)
          }

          case BInject => args match {
            case Seq(arg) => {
              val argT = generate(arg)
              res = Seq(
                arg.typ -> argT,
                expr.typ -> arg.typ
              ) ++ res
              expr.typ
            }
            case _ => throw Errors.ArityMismatch(BInject, args.length, 1)
          }
        }
      }

      case EIf(pred, tru, fls) => {
        val predT = generate(pred)
        val truT = generate(tru)
        val flsT = generate(fls)
        res = Seq(
          predT -> TBool,
          truT -> flsT,
          expr.typ -> truT
        ) ++ res
        expr.typ
      }

      case EBlock(exprs) => {
        exprs.foldLeft[Type](TUnit) {
          case (_, expr) => generate(expr)
        }
      }
    }

    generate(expr)(initEnv)
    res
  }

  def unify(constraint: Constraint): Subst = constraint match {
    case (m1@TMetavar(id1), m2@TMetavar(id2)) => if (id1 == id2) {
      Subst.empty()
    } else {
      Subst.singleton(m1, m2)
    }
    case (meta@TMetavar(id), typ) => if (typ.contains(id)) {
      throw Errors.OccursCheckFailed(meta, typ)
    } else {
      Subst.singleton(meta, typ)
    }
    case (typ, meta@TMetavar(id)) => if (typ.contains(id)) {
      throw Errors.OccursCheckFailed(meta, typ)
    } else {
      Subst.singleton(meta, typ)
    }
    case (TNum, TNum) | (TBool, TBool) | (TString, TString) => Subst.empty()
    case (t1@TId(id1), t2@TId(id2)) => if (id1 == id2) {
      Subst.empty()
    } else {
      Subst.singleton(t1, t2)
    }
    case (TFun(argT1, resT1), TFun(argT2, resT2)) => {
      val subst = unify(argT1 -> argT2)
      subst.compose(unify(subst(resT1) -> subst(resT2)))
    }
    case (t1, t2) => throw Errors.UnificationFailed(t1, t2)
  }

  def unifyAll(constraints: Seq[Constraint]): Subst = constraints match {
    case Seq() => Subst.empty()
    case constraint +: constraints => {
      val subst = unify(constraint)
      val constraintsPrime = constraints.map {
        case (t1, t2) => (subst(t1), subst(t2))
      }
      subst.compose(unifyAll(constraintsPrime))
    }
  }

  def infer(expr: Expr, initEnv: TEnv): Subst = {
    val constraints = generateConstraints(expr, initEnv)
    val subst = unifyAll(constraints)
    subst
  }

  def infer(stmt: Statement, initEnv: TEnv): (Subst, TEnv) = {
    val (constraints, env) = generateConstraints(stmt, initEnv)
    val subst = unifyAll(constraints)
    (subst, env)
  }
}
