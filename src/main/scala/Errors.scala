package coke

import Syntax._

object Errors {
  // TODO: change this to eventually take types, not strings.
  case class InvalidArgument(op: Op2, found: String, expected: String) extends RuntimeException(
    s"Invalid use of operator $op. Expected: $expected, found: $found."
  )

  case class UnboundIdentifier(id: String) extends RuntimeException(
    s"Found unbound identifier: $id."
  )

  case class ParsingFailed(msg: String) extends RuntimeException(s"Parsing failed:\n$msg")

  case class InvalidREPLCommand(cmd: String) extends RuntimeException(
    s"Invalid REPL command: $cmd"
  )

  case object MissingMain extends RuntimeException(s"No main function found.")
  case object Unreachable extends RuntimeException(s"This point should be unreachable!")
}
