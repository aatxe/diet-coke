package coke

import scala.util.{Try, Success, Failure}

object Syntax {
  type Env = Map[String, Value]

  case class LanguageException(msg: String, level: Int) extends RuntimeException(s"($level): $msg")

  sealed trait Value {
    def typeString: String = this match {
      case VUnit => "()"
      case VConst(c) => c.typ.pretty
      case VFix(_, _) => "fixpoint"
      case VClosure(_, _, _) => "function"
      case VThunk(_, _) => "thunk"
      case VExpr(_) => "expression"
    }

    lazy val pretty: String = Pretty.prettyValue(this)

    var typ: Type = TVar("a")
    def setType(typ: Type): Value = {
      this.typ = typ
      this
    }
  }

  case object VUnit extends Value
  case class VConst(c: Const) extends Value
  case class VFix(id: String, expr: Expr) extends Value
  case class VClosure(id: String, body: Expr, env: Env) extends Value
  case class VThunk(body: Seq[Expr], env: Env) extends Value
  case class VExpr(e: Expr) extends Value

  sealed trait Const {
    lazy val typ: Type = this match {
      case CNum(_) => TNum
      case CBool(_) => TBool
      case CString(_) => TString
    }

    lazy val pretty: String = Pretty.prettyConst(this)
  }

  case class CNum(n: Int) extends Const
  case class CBool(b: Boolean) extends Const
  case class CString(s: String) extends Const

  // Operations

  sealed trait Op {
    val pretty: String
    val typ: Type
  }

  // Unary operations

  sealed trait Op1 extends Op {
    def apply(arg: Const): Const

    override lazy val pretty: String = Pretty.prettyOp1(this)
  }


  case object ONot extends Op1 {
    override val typ = TFun(TBool, TRowEmpty, TBool)

    override def apply(arg: Const): Const = arg match {
      case CBool(b) => CBool(!b)
      case bad => throw Errors.InvalidOpArgument(ONot, bad.typ.pretty, "bool")
    }
  }

  case object ONeg extends Op1 {
    override val typ = TFun(TNum, TRowEmpty, TNum)

    override def apply(arg: Const): Const = arg match {
      case CNum(n) => CNum(-n)
      case bad => throw Errors.InvalidOpArgument(ONeg, bad.typ.pretty, "num")
    }
  }

  sealed trait Op2 extends Op {
    def apply(lhs: Const, rhs: Const): Const

    override lazy val pretty: String = Pretty.prettyOp2(this)
  }

  // Binary operations on numbers yielding numbers.

  case object OAdd extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TNum))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m + n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OAdd, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OAdd, bad.typ.pretty, "num")
    }
  }

  case object OSub extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TNum))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m - n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OSub, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OSub, bad.typ.pretty, "num")
    }
  }

  case object OMul extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TNum))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m * n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OMul, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OMul, bad.typ.pretty, "num")
    }
  }

  case object ODiv extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TNum))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m / n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(ODiv, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(ODiv, bad.typ.pretty, "num")
    }
  }

  case object OMod extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TNum))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m % n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OMod, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OMod, bad.typ.pretty, "num")
    }
  }

  // Binary operations on numbers yielding booleans

  case object OLt extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m < n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OLt, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OLt, bad.typ.pretty, "num")
    }
  }

  case object OLte extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m <= n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OLte, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OLte, bad.typ.pretty, "num")
    }
  }

  case object OGt extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m > n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OGt, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OGt, bad.typ.pretty, "num")
    }
  }

  case object OGte extends Op2 {
    override val typ = TFun(TNum, TRowEmpty, TFun(TNum, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m >= n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OGte, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OGte, bad.typ.pretty, "num")
    }
  }

  case object OEq extends Op2 {
    val argTyp = TVar("a")
    override val typ = TFun(argTyp, TRowEmpty, TFun(argTyp, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m == n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OEq, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OEq, bad.typ.pretty, "num")
    }
  }

  case object ONEq extends Op2 {
    val argTyp = TVar("a")
    override val typ = TFun(argTyp, TRowEmpty, TFun(argTyp, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m != n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(ONEq, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(ONEq, bad.typ.pretty, "num")
    }
  }

  // Binary operations on strings yielding strings

  case object OConcat extends Op2 {
    override val typ = TFun(TString, TRowEmpty, TFun(TString, TRowEmpty, TString))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CString(m), CString(n)) => CString(m + n)
      case (CString(_), bad) => throw Errors.InvalidOpArgument(OConcat, bad.typ.pretty, "string")
      case (bad, _) => throw Errors.InvalidOpArgument(OConcat, bad.typ.pretty, "string")
    }
  }

  // Binary operations on booleans yielding booleans

  case object OAnd extends Op2 {
    override val typ = TFun(TBool, TRowEmpty, TFun(TBool, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m && n)
      case (CBool(_), bad) => throw Errors.InvalidOpArgument(OAnd, bad.typ.pretty, "bool")
      case (bad, _) => throw Errors.InvalidOpArgument(OAnd, bad.typ.pretty, "bool")
    }
  }

  case object OOr extends Op2 {
    override val typ = TFun(TBool, TRowEmpty, TFun(TBool, TRowEmpty, TBool))

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m || n)
      case (CBool(_), bad) => throw Errors.InvalidOpArgument(OOr, bad.typ.pretty, "bool")
      case (bad, _) => throw Errors.InvalidOpArgument(OOr, bad.typ.pretty, "bool")
    }
  }

  // Built-in functions

  sealed trait BuiltIn {
    val typ: Type

    def apply(args: Seq[Value]): Value
  }

  case object BShow extends BuiltIn {
    override val typ = TFun(TVar("a"), TRowEmpty, TString)

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(value) => VConst(CString(value.pretty))
      case _ => throw Errors.ArityMismatch(BShow, args.length, 1)
    }
  }

  case object BPrint extends BuiltIn {
    override val typ = TFun(TString, TRowExtend("io", TUnit, TRowEmpty), TUnit)

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => print(str); VUnit
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BPrint, bad.typeString, "string")
      case _ => throw Errors.ArityMismatch(BPrint, args.length, 1)
    }
  }

  case object BPrintln extends BuiltIn {
    override val typ = TFun(TString, TRowExtend("io", TUnit, TRowEmpty), TUnit)

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => println(str); VUnit
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BPrintln, bad.typeString, "string")
      case _ => throw Errors.ArityMismatch(BPrintln, args.length, 1)
    }
  }

  case object BCatch extends BuiltIn {
    val effect = TVar()
    val exnEffect = TRowExtend("exn", TUnit, effect)
    val argTyp = TVar("a")
    override val typ = TFun(TFun(TUnit, exnEffect, argTyp), TRowEmpty, TFun(TFun(TString, effect, argTyp), effect, argTyp))

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(thunk@VThunk(_, _), VClosure(id, exnBody, exnEnv)) => Try(Interpreter.evalThunk(thunk)) match {
        case Success(value) => value
        case Failure(LanguageException(msg, 0)) => Interpreter.evalExpr(exnBody)(exnEnv + (id -> VConst(CString(msg))))
        case Failure(LanguageException(msg, n)) => throw LanguageException(msg, n - 1)
        case Failure(exn) => throw exn
      }
      case Seq(VThunk(_, _), bad) => throw Errors.InvalidBuiltInArgument(BCatch, bad.typeString, "function")
      case Seq(bad, _) => throw Errors.InvalidBuiltInArgument(BCatch, bad.typeString, "thunk")
      case _ => throw Errors.ArityMismatch(BCatch, args.length, 2)
    }
  }

  case object BInject extends BuiltIn {
    val effect = TVar()
    val exnEffect = TRowExtend("exn", TUnit, effect)
    val exnExnEffect = TRowExtend("exn", TUnit, exnEffect)
    val argTyp = TVar("a")
    override val typ = TFun(TFun(TUnit, exnEffect, argTyp), TRowEmpty, TFun(TUnit, exnExnEffect, argTyp))

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(thunk@VThunk(_, _)) => Try(Interpreter.evalThunk(thunk)) match {
        case Success(value) => value
        case Failure(LanguageException(msg, n)) => throw LanguageException(msg, n + 1)
        case Failure(exn) => throw exn
      }
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BInject, bad.typeString, "thunk")
      case _ => throw Errors.ArityMismatch(BInject, args.length, 1)
    }
  }

  case object BRandom extends BuiltIn {
    val effect = TVar()
    val ndetEffect = TRowExtend("ndet", TUnit, effect)
    override val typ = TFun(TUnit, ndetEffect, TNum)

    override def apply(args: Seq[Value]): Value = args match {
      case Seq() => VConst(CNum(scala.util.Random.nextInt))
      case _ => throw Errors.ArityMismatch(BRandom, args.length, 0)
    }
  }

  // Types

  sealed trait Kind {
    def pretty: String = Pretty.prettyKind(this)
  }

  case object KStar extends Kind
  case object KRow extends Kind

  case class TyVar(name: String, kind: Kind, lacks: Set[String])
  case class Scheme(tyvars: Seq[TyVar], typ: Type)

  type TEnv = Map[String, Scheme]
  def emptyTypeEnv(): TEnv = Map()

  type Effect = Type

  sealed trait Type {
    lazy val pretty: String = Pretty.prettyType(this)

    lazy val freeTypeVars: Set[TyVar] = this match {
      case TUnit | TNum | TString | TBool | TRowEmpty => Set()
      case TVar(v) => Set(v)
      case TFun(lhs, eff, rhs) => lhs.freeTypeVars union eff.freeTypeVars union rhs.freeTypeVars
      case TRowExtend(_, lhs, rhs) => lhs.freeTypeVars union rhs.freeTypeVars
    }
  }

  case object TUnit extends Type
  case object TNum extends Type
  case object TString extends Type
  case object TBool extends Type
  case class TVar(v: TyVar) extends Type
  case class TFun(lhs: Type, eff: Effect , rhs: Type) extends Type

  case object TRowEmpty extends Type
  case class TRowExtend(label: String, lhs: Type, rhs: Type) extends Type

  // Companion object for producing fresh type variables.
  object TVar {
    var count = 0

    def apply(name: String, kind: Kind, lacks: Set[String]): TVar = {
      val res = TVar(TyVar(s"$name$count", kind, lacks))
      count += 1
      res
    }

    def apply(name: String): TVar = TVar(name, KStar, Set())

    def apply(lacks: Set[String]): TVar = TVar("e", KRow, lacks)

    def apply(): TVar = TVar(Set[String]())
  }

  // Expressions

  sealed trait Expr {
    lazy val pretty: String = Pretty.prettyExpr(this)

    def infer(implicit env: TEnv): (Type, Effect) = InferenceEngine.infer(this) match {
      case (_, t, e) => (t, e)
    }
  }

  case object EUnit extends Expr
  case class EError(msg: String) extends Expr
  case class EId(id: String) extends Expr
  case class EConst(c: Const) extends Expr
  case class EOp1(op1: Op1, expr: Expr) extends Expr
  case class EOp2(op2: Op2, lhs: Expr, rhs: Expr) extends Expr
  case class EFix(id: String, expr: Expr) extends Expr
  case class EFun(id: String, body: Expr) extends Expr
  case class EApp(fun: Expr, arg: Expr) extends Expr
  case class EBuiltIn(builtIn: BuiltIn, args: Seq[Expr]) extends Expr
  case class EIf(pred: Expr, tru: Expr, fls: Expr) extends Expr
  case class EBlock(exprs: Seq[Expr]) extends Expr

  // Statements

  sealed trait Statement {
    def infer(implicit env: TEnv): (TEnv, Type, Effect) = this match {
      case SBinding(id, body) => {
        val envPrime = env + (id -> Scheme(Seq(), TVar("a")))
        body.infer(envPrime) match {
          case (t, e) => (envPrime + (id -> Scheme(Seq(), t)), t, e)
        }
      }
      case SExpr(expr) => expr.infer match {
        case (t, e) => (env, t, e)
      }
      case SBlock(_) => ???
    }
  }

  case class SBinding(id: String, body: Expr) extends Statement
  case class SExpr(expr: Expr) extends Statement
  case class SBlock(stmts: Seq[Statement]) extends Statement
}
