import scala.util.parsing.combinator._

class ExprParser extends RegexParsers {
  val digit = "[0-9]+".r

  // Can't discard the "+" or "-" since they are needed in the match
  def expression: Parser[Int] = term ~ opt(("+" | "-" ) ~ expression) ^^ {
    case t ~ None => t
    case t ~ Some("+" ~ e) => t + e
    case t ~ Some("-" ~ e) => t - e
  }

  def term: Parser[Int] = factor ~ rep("/"  ~> factor ) ^^ {
    case f ~ r => f / r.product
  }

  def factor: Parser[Int] = digit ^^ { _.toInt } | "(" ~> expression <~ ")"
}

object Main extends App {
  val parser = new ExprParser
  val result = parser.parseAll(parser.expression, "10/2+5")
  if (result.successful) println(result.get)
  if(!result.successful) println("failure")
}

