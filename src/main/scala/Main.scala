package coke

import scala.io.StdIn
import scala.util.{Try, Success, Failure}

object Main extends App {
  val prompt = "\u03bb "
  val altPrompt = "| "

  var running = true
  var multiline = false
  implicit var env: Syntax.Env = Map()
  while (running) {
    print(prompt)

    Try({
      val input = if (multiline == false) {
        StdIn.readLine()
      } else {
        var buffer = StdIn.readLine()
        var line = buffer
        while (line != "" && buffer.startsWith(":") == false) {
          print(altPrompt)
          line = StdIn.readLine()
          buffer += "\n" + line
        }
        buffer
      }

      if (input.startsWith(":")) {
        parseCommand(input) match {
          case CQuit => running = false
          case CMulti(enable) => multiline = enable
        }
      } else {
        val prog = Parser.parse(input)

        // Update the environment.
        env = Interpreter.getUpdatedEnv(prog)

        // Evaluate program in updated environment.
        val res = Interpreter.evalStatement(prog)

        println(res)
      }
    }) match {
      case Success(_) => ()
      case Failure(exn) => println(exn.getMessage)
    }
  }

  def parseCommand(str: String): Command = str.substring(1).split(' ').toSeq match {
    case Seq("quit") | Seq("exit") => CQuit
    case Seq("multiline", "on") | Seq("m", "on") => CMulti(true)
    case Seq("multiline", "off") | Seq("m", "off") => CMulti(false)
    case cmd +: _ => throw Errors.InvalidREPLCommand(cmd)
    case Seq() => throw Errors.Unreachable
  }

  sealed trait Command
  case object CQuit extends Command
  case class CMulti(enable: Boolean) extends Command
}
