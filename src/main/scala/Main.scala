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
  implicit var typeEnv: Syntax.TEnv = Map()
  implicit var subst: Subst = Subst.empty()
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
          case CType(id) => typeEnv.get(id) match {
            case Some(typ) => println(s"$id :: ${typ.pretty}")
            case None => println(s"$id is unbound in the type environment.")
          }
        }
      } else {
        val prog = Parser.parse(input)

        // Run type inference on the program.
        val (substPrime, typeEnvPrime) = prog.infer(typeEnv)
        subst = subst.compose(substPrime)

        // Update the type environment.
        typeEnv = env.mapValues(value => value.typ) ++ typeEnvPrime
        typeEnv = typeEnv.mapValues(typ => subst(typ))

        // Update the environment.
        env = Interpreter.getUpdatedEnv(prog)

        // Evaluate program in updated environment.
        try {
          val res = Interpreter.evalStatement(prog).setType(subst(prog.typ))
          if (res != Syntax.VUnit) {
            println(s"let res$resNum: ${res.typ.pretty} = ${res.pretty}")
            env = env + (s"res$resNum" -> res)
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
    case Seq("type", id) => CType(id)
    case Seq("t", id) => CType(id)
    case cmd +: _ => throw Errors.InvalidREPLCommand(cmd)
    case Seq() => throw Errors.Unreachable
  }

  sealed trait Command
  case object CQuit extends Command
  case class CMulti(enable: Boolean) extends Command
  case class CType(id: String) extends Command
}
