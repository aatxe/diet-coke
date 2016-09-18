package coke

object Syntax {
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

  sealed trait Expr
  case class EId(id: String) extends Expr
  case class EConst(c: Const) extends Expr
  case class EOp2(op2: Op2, lhs: Expr, rhs: Expr) extends Expr

  sealed trait Statement
  case class SBinding(id: String, body: Expr) extends Statement
}
