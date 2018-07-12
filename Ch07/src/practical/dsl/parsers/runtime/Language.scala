package practical.dsl.parsers.runtime

import scala.io.Source

import practical.dsl.parsers.Translator

object Language {
  def main(args: Array[String]) {
    val inputFile = Source.fromFile("source/practical.bascala")
    val inputSource = inputFile.mkString

    val parser = new SmallLanguageParser
    parser.parseAll(parser.program, inputSource) match {
      case parser.Success(r, n) => {
        val interpreter = new Interpreter(r)

        try {
          interpreter.run
        } catch {
          case e: RuntimeException => println(e.getMessage)
        }
      }
      case parser.Error(msg, n) => println("Error: " + msg)
      case parser.Failure(msg, n) => println("Error: " + msg)
      case _ =>
    }
  }
}