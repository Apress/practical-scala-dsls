package practical.dsl.parsers.model

import scala.collection.mutable.HashMap
import scala.util.parsing.input.Positional

case class CallFunctionMethod(name: String, values: Map[String, Expr]) extends Expr with Statement