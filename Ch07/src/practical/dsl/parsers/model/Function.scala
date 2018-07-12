package practical.dsl.parsers.model

import scala.collection.mutable.HashMap

case class Function(name: String, arguments: Map[String, Int], statements: List[Statement], val rtnValue: Expr)