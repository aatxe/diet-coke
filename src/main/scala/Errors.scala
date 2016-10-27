package coke

import Syntax._

object Errors {
  // TODO: change this to eventually take types, not strings.
  case class InvalidOpArgument(op: Op, found: String, expected: String) extends RuntimeException(
    s"Invalid use of operator $op. Expected: $expected, found: $found."
  )

  case class InvalidBuiltInArgument(builtIn: BuiltIn, found: String, expected: String) extends RuntimeException(
    s"Invalid use of $builtIn. Expected: $expected, found: $found."
  )

  case class ArityMismatch(builtIn: BuiltIn, found: Int, expected: Int) extends RuntimeException(
    s"Encountered an arity mismatch for $builtIn. Expected $expected arguments, found $found."
  )

  case class UnboundIdentifier(id: String) extends RuntimeException(
    s"Found unbound identifier: $id."
  )

  case class ParsingFailed(msg: String) extends RuntimeException(s"Parsing failed:\n$msg")

  case class InvalidREPLCommand(cmd: String) extends RuntimeException(
    s"Invalid REPL command: $cmd"
  )

  case object RecursiveRowFound extends RuntimeException(s"Unification failed after finding a recursive row.")
  case class RowContainedIllegalLabels(typ: Type, labels: Set[String]) extends RuntimeException(s"Unification failed because ${typ.pretty} contained illegal labels ($labels).")
  case class RowRewriteFailed(typ: Type, label: String) extends RuntimeException(s"Unification failed because the row ${typ.pretty} cannot be rewritten as an extension of label $label.")
  case class OccursCheckFailed(lhs: Type, rhs: Type) extends RuntimeException(s"Occurs check failed during unification. Type ${lhs.pretty} occurred in type ${rhs.pretty}.")
  case class UnificationFailed(lhs: Type, rhs: Type) extends RuntimeException(s"Unification failed due to conflicting constraints. (${lhs.pretty} ~ ${rhs.pretty})")
  case class KindMismatch(lhs: Kind, rhs: Kind) extends RuntimeException(s"Unification failed due to a kind mismatch. (${lhs.pretty} ~ ${rhs.pretty})")

  case object StackOverflow extends RuntimeException(s"Execution overflowed the call stack.")
  case object MissingMain extends RuntimeException(s"No main function found.")
  case object Unreachable extends RuntimeException(s"This point should be unreachable!")
}
