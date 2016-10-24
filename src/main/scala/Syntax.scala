package coke

import scala.util.{Try, Success, Failure}

object Syntax {
  type Env = Map[String, Value]

  case class LanguageException(msg: String, level: Int) extends RuntimeException(s"($level): $msg")

  sealed trait Value {
    def typeString: String = this match {
      case VUnit => "()"
      case VConst(c) => c.typ.pretty
      case VClosure(_, _, _) => "function"
      case VThunk(_, _) => "thunk"
      case VExpr(_) => "expression"
    }

    lazy val pretty: String = Pretty.prettyValue(this)
  }

  case object VUnit extends Value
  case class VConst(c: Const) extends Value
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
    override val typ = TBool

    override def apply(arg: Const): Const = arg match {
      case CBool(b) => CBool(!b)
      case bad => throw Errors.InvalidOpArgument(ONot, bad.typ.pretty, "bool")
    }
  }

  case object ONeg extends Op1 {
    override val typ = TNum

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
    override val typ = TNum

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m + n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OAdd, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OAdd, bad.typ.pretty, "num")
    }
  }

  case object OSub extends Op2 {
    override val typ = TNum

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m - n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OSub, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OSub, bad.typ.pretty, "num")
    }
  }

  case object OMul extends Op2 {
    override val typ = TNum

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match { 
      case (CNum(m), CNum(n)) => CNum(m * n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OMul, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OMul, bad.typ.pretty, "num")
    }
  }

  case object ODiv extends Op2 {
    override val typ = TNum

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m / n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(ODiv, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(ODiv, bad.typ.pretty, "num")
    }
  }

  case object OMod extends Op2 {
    override val typ = TNum

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m % n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OMod, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OMod, bad.typ.pretty, "num")
    }
  }

  // Binary operations on numbers yielding booleans

  case object OLt extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m < n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OLt, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OLt, bad.typ.pretty, "num")
    }
  }

  case object OLte extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m <= n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OLte, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OLte, bad.typ.pretty, "num")
    }
  }

  case object OGt extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m > n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OGt, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OGt, bad.typ.pretty, "num")
    }
  }

  case object OGte extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m >= n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OGte, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OGte, bad.typ.pretty, "num")
    }
  }

  case object OEq extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m == n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(OEq, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(OEq, bad.typ.pretty, "num")
    }
  }

  case object ONEq extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m != n)
      case (CNum(_), bad) => throw Errors.InvalidOpArgument(ONEq, bad.typ.pretty, "num")
      case (bad, _) => throw Errors.InvalidOpArgument(ONEq, bad.typ.pretty, "num")
    }
  }

  // Binary operations on strings yielding strings

  case object OConcat extends Op2 {
    override val typ = TString

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CString(m), CString(n)) => CString(m + n)
      case (CString(_), bad) => throw Errors.InvalidOpArgument(OConcat, bad.typ.pretty, "string")
      case (bad, _) => throw Errors.InvalidOpArgument(OConcat, bad.typ.pretty, "string")
    }
  }

  // Binary operations on booleans yielding booleans

  case object OAnd extends Op2 {
    override val typ = TBool

    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m && n)
      case (CBool(_), bad) => throw Errors.InvalidOpArgument(OAnd, bad.typ.pretty, "bool")
      case (bad, _) => throw Errors.InvalidOpArgument(OAnd, bad.typ.pretty, "bool")
    }
  }

  case object OOr extends Op2 {
    override val typ = TBool

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
    override val typ = TString

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(value) => VConst(CString(value.pretty))
      case _ => throw Errors.ArityMismatch(BShow, args.length, 1)
    }
  }

  case object BPrint extends BuiltIn {
    override val typ = TUnit

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => print(str); VUnit
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BPrint, bad.typeString, "string")
      case _ => throw Errors.ArityMismatch(BPrint, args.length, 1)
    }
  }

  case object BPrintln extends BuiltIn {
    override val typ = TUnit

    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => println(str); VUnit
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BPrintln, bad.typeString, "string")
      case _ => throw Errors.ArityMismatch(BPrintln, args.length, 1)
    }
  }

  case object BCatch extends BuiltIn {
    override val typ = TMetavar()

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
    override val typ = TMetavar()

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

  // Types

  sealed trait Type {
    lazy val pretty: String = Pretty.prettyType(this)
  }

  case object TUnit extends Type
  case object TNum extends Type
  case object TString extends Type
  case object TBool extends Type
  case class TMetavar private(id: Int) extends Type
  case class TId(id: String) extends Type
  case class TFun(lhs: Type, rhs: Type) extends Type

  // Companion object for TMetavar to add constructor for fresh metavars.
  object TMetavar {
    private var nextId = 0

    def apply(): TMetavar = {
      val res = TMetavar(nextId)
      nextId += 1
      res
    }
  }

  // Expressions

  sealed trait Expr {
    lazy val pretty: String = Pretty.prettyExpr(this)

    var typ: Type

    def setType(typ: Type): Expr = {
      this.typ = typ
      this
    }
  }

  case object EUnit extends Expr {
    var typ: Type = TUnit
  }

  case class EError(msg: String) extends Expr {
    var typ: Type = TUnit
  }

  case class EId(id: String) extends Expr {
    var typ: Type = TMetavar()
  }

  case class EConst(c: Const) extends Expr {
    var typ: Type = c.typ
  }

  case class EOp1(op1: Op1, expr: Expr) extends Expr {
    var typ: Type = op1.typ
  }

  case class EOp2(op2: Op2, lhs: Expr, rhs: Expr) extends Expr {
    var typ: Type = op2.typ
  }

  case class EFun(id: String, body: Expr) extends Expr {
    var typ: Type = TFun(TMetavar(), body.typ)
  }

  case class EApp(fun: Expr, arg: Expr) extends Expr {
    var typ: Type = TMetavar()
  }

  case class EBuiltIn(builtIn: BuiltIn, args: Seq[Expr]) extends Expr {
    var typ: Type = builtIn.typ
  }

  case class EIf(pred: Expr, tru: Expr, fls: Expr) extends Expr {
    var typ: Type = TMetavar()
  }

  case class EBlock(exprs: Seq[Expr]) extends Expr {
    var typ: Type = exprs.last.typ
  }

  // Statements

  sealed trait Statement
  case class SBinding(id: String, body: Expr) extends Statement
  case class SExpr(expr: Expr) extends Statement
  case class SBlock(stmts: Seq[Statement]) extends Statement
}
