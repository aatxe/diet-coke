package coke

import scala.util.{Try, Success, Failure}

object Syntax {
  type Env = Map[String, Value]

  case class LanguageException(msg: String, level: Int) extends RuntimeException(s"($level): $msg")

  sealed trait Value {
    def typeString: String

    lazy val pretty: String = Pretty.prettyValue(this)
  }

  case object VUnit extends Value {
    def typeString: String = "()"
  }

  case class VConst(c: Const) extends Value {
    def typeString: String = c.typeString
  }

  case class VClosure(id: String, body: Expr, env: Env) extends Value {
    def typeString: String = "function"
  }

  case class VThunk(body: Seq[Expr], env: Env) extends Value {
    def typeString: String = "thunk"
  }

  case class VExpr(e: Expr) extends Value {
    def typeString: String = "expression"
  }

  sealed trait Const {
    def typeString: String

    lazy val pretty: String = Pretty.prettyConst(this)
  }

  case class CNum(n: Int) extends Const {
    def typeString: String = "num"
  }

  case class CBool(b: Boolean) extends Const {
    def typeString: String = "bool"
  }

  case class CString(s: String) extends Const {
    def typeString: String = "string"
  }

  sealed trait Op2 {
    def apply(lhs: Const, rhs: Const): Const

    lazy val pretty: String = Pretty.prettyOp2(this)
  }

  // Binary operations on numbers yielding numbers.

  case object OAdd extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m + n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OAdd, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OAdd, bad.typeString, "num")
    }
  }

  case object OSub extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m - n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OSub, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OSub, bad.typeString, "num")
    }
  }

  case object OMul extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match { 
      case (CNum(m), CNum(n)) => CNum(m * n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OMul, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OMul, bad.typeString, "num")
    }
  }

  case object ODiv extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m / n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(ODiv, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(ODiv, bad.typeString, "num")
    }
  }

  case object OMod extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m % n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OMod, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OMod, bad.typeString, "num")
    }
  }

  // Binary operations on numbers yielding booleans

  case object OLt extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m < n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OLt, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OLt, bad.typeString, "num")
    }
  }

  case object OLte extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m <= n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OLte, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OLte, bad.typeString, "num")
    }
  }

  case object OGt extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m > n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OGt, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OGt, bad.typeString, "num")
    }
  }

  case object OGte extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m >= n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OGte, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OGte, bad.typeString, "num")
    }
  }

  case object OEq extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m == n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(OEq, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(OEq, bad.typeString, "num")
    }
  }

  case object ONEq extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m != n)
      case (CNum(_), bad) => throw Errors.InvalidArgument(ONEq, bad.typeString, "num")
      case (bad, _) => throw Errors.InvalidArgument(ONEq, bad.typeString, "num")
    }
  }

  // Binary operations on strings yielding strings

  case object OConcat extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CString(m), CString(n)) => CString(m + n)
      case (CString(_), bad) => throw Errors.InvalidArgument(OConcat, bad.typeString, "string")
      case (bad, _) => throw Errors.InvalidArgument(OConcat, bad.typeString, "string")
    }
  }

  // Binary operations on booleans yielding booleans

  case object OAnd extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m && n)
      case (CBool(_), bad) => throw Errors.InvalidArgument(OAnd, bad.typeString, "bool")
      case (bad, _) => throw Errors.InvalidArgument(OAnd, bad.typeString, "bool")
    }
  }

  case object OOr extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m || n)
      case (CBool(_), bad) => throw Errors.InvalidArgument(OOr, bad.typeString, "bool")
      case (bad, _) => throw Errors.InvalidArgument(OOr, bad.typeString, "bool")
    }
  }

  // Built-in functions

  sealed trait BuiltIn {
    def apply(args: Seq[Value]): Value
  }

  case object BShow extends BuiltIn {
    override def apply(args: Seq[Value]): Value = args match {
      case Seq(value) => VConst(CString(value.toString))
      case _ => throw Errors.ArityMismatch(BShow, args.length, 1)
    }
  }

  case object BPrint extends BuiltIn {
    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => print(str); VUnit
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BPrint, bad.typeString, "string")
      case _ => throw Errors.ArityMismatch(BPrint, args.length, 1)
    }
  }

  case object BPrintln extends BuiltIn {
    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => println(str); VUnit
      case Seq(bad) => throw Errors.InvalidBuiltInArgument(BPrintln, bad.typeString, "string")
      case _ => throw Errors.ArityMismatch(BPrintln, args.length, 1)
    }
  }

  case object BCatch extends BuiltIn {
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

  sealed trait Expr {
    lazy val pretty: String = Pretty.prettyExpr(this)
  }

  case object EUnit extends Expr
  case class EError(msg: String) extends Expr
  case class EId(id: String) extends Expr
  case class EConst(c: Const) extends Expr
  case class EOp2(op2: Op2, lhs: Expr, rhs: Expr) extends Expr
  case class EFun(id: String, body: Expr) extends Expr
  case class EApp(fun: Expr, arg: Expr) extends Expr
  case class EBuiltIn(builtIn: BuiltIn, args: Seq[Expr]) extends Expr
  case class EIf(pred: Expr, tru: Expr, fls: Expr) extends Expr
  case class EBlock(exprs: Seq[Expr]) extends Expr

  sealed trait Statement
  case class SBinding(id: String, body: Expr) extends Statement
  case class SExpr(expr: Expr) extends Statement
  case class SBlock(stmts: Seq[Statement]) extends Statement
}
