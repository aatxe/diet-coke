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
    case SBlock(stmts) => stmts.foldLeft(env) {
      case (acc, stmt) => getUpdatedEnv(stmt)(acc)
    }
  }

  def evalStatement(stmt: Statement)(implicit env: Env): Value = stmt match {
    case SBinding(_, _) => VUnit
    case SExpr(expr) => evalExpr(expr)
    case SBlock(stmts) => stmts.foldLeft[(Value, Env)]((VUnit, env)) {
      case ((_, envPrime), stmt) => (evalStatement(stmt), getUpdatedEnv(stmt)(envPrime))
    }._1
  }

  def evalExpr(expr: Expr)(implicit env: Env): Value = expr match {
    case EUnit => VUnit
    case EError(msg) => throw LanguageException(msg, 0)
    case EId(id) => env.getOrElse(id, throw UnboundIdentifier(id))
    case EConst(c) => VConst(c)
    case EFun(id, body) => VClosure(id, body, env)
    case EOp1(op, arg) => evalOp1(op, evalExpr(arg))
    case EOp2(op, lhs, rhs) => evalOp2(op, evalExpr(lhs), evalExpr(rhs))
    case EApp(fun, arg) => evalApp(evalExpr(fun), evalExpr(arg))
    case EBuiltIn(builtIn, args) => evalBuiltIn(builtIn, args.map(evalExpr(_)))
    case EIf(pred, tru, fls) => evalIf(evalExpr(pred), tru, fls)
    case EBlock(exprs) => VThunk(exprs, env)
  }

  def evalOp1(op: Op1, arg: Value)(implicit env: Env): Value = arg match {
    case VConst(arg) => VConst(op(arg))
    case VExpr(expr) => evalOp1(op, evalExpr(expr))
    case _ => ???
  }

  def evalOp2(op: Op2, lhs: Value, rhs: Value)(implicit env: Env): Value = (lhs, rhs) match {
    case (VConst(lhs), VConst(rhs)) => VConst(op(lhs, rhs))
    case (VExpr(lhs), rhs) => evalOp2(op, evalExpr(lhs), rhs)
    case (lhs, VExpr(rhs)) => evalOp2(op, lhs, evalExpr(rhs))
    case _ => ???
  }

  def evalThunk(thunk: VThunk): Value = thunk.body.foldLeft[Value](VUnit) {
    case (_, expr) => evalExpr(expr)(thunk.env)
  }

  def evalApp(fun: Value, arg: Value)(implicit env: Env): Value = fun match {
    case VClosure(id, body, envLocal) => evalExpr(body)(envLocal + (id -> arg))
    case VExpr(fun) => evalApp(evalExpr(fun), arg)
    case _ => ???
  }

  def evalBuiltIn(builtIn: BuiltIn, args: Seq[Value])(implicit env: Env): Value = {
    builtIn(args.map {
      case VExpr(expr) => evalExpr(expr)
      case value => value
    })
  }

  def evalIf(pred: Value, tru: Expr, fls: Expr)(implicit env: Env): Value = pred match {
    case VConst(CBool(true)) => evalExpr(tru)
    case VConst(CBool(false)) => evalExpr(fls)
    case VExpr(pred) => evalIf(evalExpr(pred), tru, fls)
    case _ => ???
  }
}
