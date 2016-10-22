package coke

object Syntax {
  type Env = Map[String, Value]

  sealed trait Value
  case object VUnit extends Value
  case class VConst(c: Const) extends Value
  case class VClosure(id: String, body: Expr, env: Env) extends Value
  case class VExpr(e: Expr) extends Value

  sealed trait Const
  case class CNum(n: Int) extends Const
  case class CBool(b: Boolean) extends Const
  case class CString(s: String) extends Const

  sealed trait Op2 {
    def apply(lhs: Const, rhs: Const): Const
  }

  case object OAdd extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m + n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OAdd, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OAdd, "string", "num")
    }
  }

  case object OSub extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m - n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OSub, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OSub, "string", "num")
    }
  }

  case object OMul extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m * n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OMul, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OMul, "string", "num")
    }
  }

  case object ODiv extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m / n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(ODiv, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(ODiv, "string", "num")
    }
  }

  case object OMod extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CNum(m % n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OMod, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OMod, "string", "num")
    }
  }

  case object OConcat extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CString(m), CString(n)) => CString(m + n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OConcat, "bool", "string")
      case (CNum(_), _) | (_, CNum(_)) => throw Errors.InvalidArgument(OConcat, "num", "string")
    }
  }

  sealed trait Expr
  case class EId(id: String) extends Expr
  case class EConst(c: Const) extends Expr
  case class EOp2(op2: Op2, lhs: Expr, rhs: Expr) extends Expr
  case class EFun(id: String, body: Expr) extends Expr
  case class EApp(fun: Expr, arg: Expr) extends Expr

  sealed trait Statement
  case class SBinding(id: String, body: Expr) extends Statement
  case class SExpr(expr: Expr) extends Statement
}
