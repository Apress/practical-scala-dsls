import scala.util.parsing.combinator._

class ImprovedJsonParser extends JavaTokenParsers {

  def obj: Parser[Map[String, Any]] =
    "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)

  def array: Parser[List[Any]] =
    "["~> repsep(value, ",") <~"]"

  def member: Parser[(String, Any)] =
    stringLiteral~":"~value ^^
      { case name~":"~value => (name, value) }

  def value: Parser[Any] = (
    obj
      | array
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
    )
}

object ImprovedJsonParserTest extends ImprovedJsonParser {
  def main(args: Array[String]) {
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
