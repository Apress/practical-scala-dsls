package practical.dsl.parsers.model

case class Variable(name: String, value: Expr) extends Statement