package coke

import scala.io.StdIn
import scala.util.{Try, Success, Failure}

object Main extends App {
  val prompt = "\u03bb "
  val altPrompt = "| "

  var running = true
  var multiline = false
  var resNum = 0
  implicit var env: Syntax.Env = Map()
  implicit var typEnv: Syntax.TEnv = Map()
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
          case CType(expr) => expr.infer match {
            case (typ, _) => println(s"${expr.pretty} :: ${typ.pretty}")
          }
        }
      } else {
        val prog = Parser.parse(input)

        // Run type inference on the program.
        val (typEnvPrime, typ, _) = prog.infer

        // Update the type environment.
        typEnv = typEnvPrime

        // Update the environment.
        env = Interpreter.getUpdatedEnv(prog)

        // Evaluate program in updated environment.
        try {
          val res = Interpreter.evalStatement(prog)
          if (res != Syntax.VUnit) {
            println(s"let res$resNum: ${typ.pretty} = ${res.pretty}")
            env = env + (s"res$resNum" -> res)
            typEnv = typEnv + (s"res$resNum" -> Syntax.Scheme(Seq(), typ))
            resNum += 1
          }
        } catch {
          case _: StackOverflowError => throw Errors.StackOverflow
        }
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
    case "type" +: input => CType(Parser.parseExpr(input.mkString(" ")))
    case "t" +: input => CType(Parser.parseExpr(input.mkString(" ")))
    case cmd +: _ => throw Errors.InvalidREPLCommand(cmd)
    case Seq() => throw Errors.Unreachable
  }

  sealed trait Command
  case object CQuit extends Command
  case class CMulti(enable: Boolean) extends Command
  case class CType(expr: Syntax.Expr) extends Command
}
