package coke

import Errors._
import Syntax._

object Interpreter {
  def buildEnv(stmts: Seq[Statement]): Env = stmts.foldRight[Env](Map()) {
    case (stmt, env) => getUpdatedEnv(stmt)(env)
  }

  def eval(stmts: Seq[Statement]): Value = {
    def isMain(stmt: Statement): Boolean = stmt match {
      case SBinding("main", _) => true
      case _ => false
    }

    implicit val env = buildEnv(stmts)
    stmts.find(isMain(_)) match {
      case Some(SBinding(_, body)) => evalExpr(body)
      case Some(_) => throw Unreachable
      case None => throw MissingMain
    }
  }

  def getUpdatedEnv(stmt: Statement)(implicit env: Env): Env = stmt match {
    case SBinding(id, body) => env + (id -> VExpr(body))
    case SExpr(_) => env
  }

  def evalStatement(stmt: Statement)(implicit env: Env): Value = stmt match {
    case SBinding(_, _) => VUnit
    case SExpr(expr) => evalExpr(expr)
  }

  def evalExpr(expr: Expr)(implicit env: Env): Value = expr match {
    case EId(id) => env.getOrElse(id, throw UnboundIdentifier(id))
    case EConst(c) => VConst(c)
    case EFun(id, body) => VClosure(id, body, env)
    case EOp2(op, lhs, rhs) => evalOp2(op, evalExpr(lhs), evalExpr(rhs))
    case EApp(fun, arg) => evalApp(evalExpr(fun), evalExpr(arg))
  }

  def evalOp2(op: Op2, lhs: Value, rhs: Value)(implicit env: Env): Value = (lhs, rhs) match {
    case (VConst(lhs), VConst(rhs)) => VConst(op(lhs, rhs))
    case (VExpr(lhs), rhs) => evalOp2(op, evalExpr(lhs), rhs)
    case (lhs, VExpr(rhs)) => evalOp2(op, lhs, evalExpr(rhs))
    case _ => ???
  }

  def evalApp(fun: Value, arg: Value)(implicit env: Env): Value = fun match {
    case VClosure(id, body, envLocal) => evalExpr(body)(envLocal + (id -> arg))
    case VExpr(fun) => evalApp(evalExpr(fun), arg)
    case _ => ???
  }
}
