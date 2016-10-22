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

  // Binary operations on numbers yielding numbers.

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

  // Binary operations on numbers yielding booleans

  case object OLt extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m < n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OLt, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OLt, "string", "num")
    }
  }


  case object OLte extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m <= n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OLte, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OLte, "string", "num")
    }
  }

  case object OGt extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m > n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OGt, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OGt, "string", "num")
    }
  }

  case object OGte extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m >= n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OGte, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OGte, "string", "num")
    }
  }

  case object OEq extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m == n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OEq, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OEq, "string", "num")
    }
  }

  case object ONEq extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CNum(m), CNum(n)) => CBool(m != n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(ONEq, "bool", "num")
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(ONEq, "string", "num")
    }
  }

  // Binary operations on strings yielding strings

  case object OConcat extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CString(m), CString(n)) => CString(m + n)
      case (CBool(_), _) | (_, CBool(_)) => throw Errors.InvalidArgument(OConcat, "bool", "string")
      case (CNum(_), _) | (_, CNum(_)) => throw Errors.InvalidArgument(OConcat, "num", "string")
    }
  }

  // Binary operations on booleans yielding booleans

  case object OAnd extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m && n)
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OAnd, "string", "bool")
      case (CNum(_), _) | (_, CNum(_)) => throw Errors.InvalidArgument(OAnd, "num", "bool")
    }
  }

  case object OOr extends Op2 {
    override def apply(lhs: Const, rhs: Const): Const = (lhs, rhs) match {
      case (CBool(m), CBool(n)) => CBool(m || n)
      case (CString(_), _) | (_, CString(_)) => throw Errors.InvalidArgument(OAnd, "string", "bool")
      case (CNum(_), _) | (_, CNum(_)) => throw Errors.InvalidArgument(OAnd, "num", "bool")
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
      case Seq(VClosure(_, _, _)) => throw Errors.InvalidBuiltInArgument(BPrint, "function", "string")
      case Seq(VConst(CBool(_))) => throw Errors.InvalidBuiltInArgument(BPrint, "bool", "string")
      case Seq(VConst(CNum(_))) => throw Errors.InvalidBuiltInArgument(BPrint, "num", "string")
      case Seq(VUnit) => throw Errors.InvalidBuiltInArgument(BPrint, "()", "string")
      case Seq(VExpr(_)) => throw Errors.InvalidBuiltInArgument(BPrint, "expression", "string")
      case _ => throw Errors.ArityMismatch(BPrint, args.length, 1)
    }
  }

  case object BPrintln extends BuiltIn {
    override def apply(args: Seq[Value]): Value = args match {
      case Seq(VConst(CString(str))) => println(str); VUnit
      case Seq(VClosure(_, _, _)) => throw Errors.InvalidBuiltInArgument(BPrint, "function", "string")
      case Seq(VConst(CBool(_))) => throw Errors.InvalidBuiltInArgument(BPrint, "bool", "string")
      case Seq(VConst(CNum(_))) => throw Errors.InvalidBuiltInArgument(BPrint, "num", "string")
      case Seq(VUnit) => throw Errors.InvalidBuiltInArgument(BPrint, "()", "string")
      case Seq(VExpr(_)) => throw Errors.InvalidBuiltInArgument(BPrint, "expression", "string")
      case _ => throw Errors.ArityMismatch(BPrint, args.length, 1)
    }
  }

  sealed trait Expr
  case object EUnit extends Expr
  case class EId(id: String) extends Expr
  case class EConst(c: Const) extends Expr
  case class EOp2(op2: Op2, lhs: Expr, rhs: Expr) extends Expr
  case class EFun(id: String, body: Expr) extends Expr
  case class EApp(fun: Expr, arg: Expr) extends Expr
  case class EBuiltIn(builtIn: BuiltIn, args: Seq[Expr]) extends Expr
  case class EIf(pred: Expr, tru: Expr, fls: Expr) extends Expr

  sealed trait Statement
  case class SBinding(id: String, body: Expr) extends Statement
  case class SExpr(expr: Expr) extends Statement
}
