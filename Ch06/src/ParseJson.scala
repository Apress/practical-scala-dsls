import scala.util.parsing.combinator._

class ParserJson extends JavaTokenParsers {
  def value : Parser[Any] = obj | array |
                            stringLiteral |
                            floatingPointNumber
  def obj : Parser[Any] = "{" ~repsep(member, ",")~"}"
  def array : Parser[Any] = "[" ~repsep(value, ",")~"]"
  def member: Parser[Any] = stringLiteral~":"~value
}

object SimpleJSONParser extends ParserJson {
  def main(args: Array[String]): Unit ={
    val reader = "{\n\t\"Username\" : " +
      "[{\"Name\"  : \"Pierluigi Riti\"," +
      "\"Roles\" : [\"Administrator\", \"User\"]," +
      "\"Groups\" : [\"Test1\",\"Test2\"]," +
      "\"Permissions\": [\"All\", \"Read\"]" +
      "}," +
      "{\"Name\" : \"John Smyth\"," +
      "\"Roles\" : [\"User\"]," +
      "\"Groups\" : [\"Test1\"]," +
      "\"Permissions\": [\"Read\"]}]} "
    println(parseAll(value, reader))
  }
}

