package practical.dsl.parsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import practical.dsl.parsers.model.{CallFunctionMethod, _}

class Reader extends StandardTokenParsers {
  lexical.reserved += ("DIM", "PRINT", "IF", "THEN", "FUNCTION","SUB", "MAIN" "RETURN", "END FUNCTION")
  lexical.delimiters += ("*", "/", "%", "+", "-", "(", ")", "=", "<", ">", "==", "!=", "<=", ">=", ",", ":")

  def mainPoint: Parser[Program] = (rep(function) <~ ("SUB" ~ "MAIN")) ~ block ^^ {
    case f ~ c => new Program(f, c)
  }

  def function: Parser[Function] = ("FUNCTION" ~> ident) ~ ("(" ~> arguments) ~ (")" ~> block) ~ opt(returnStatement) <~ "END FUNCTION" ^^ {
    case a ~ b ~ c ~ None => new Function(a, b, c, Number(0))
    case a ~ b ~ c ~ d => new Function(a, b, c, d.get)
  }

  def returnStatement: Parser[Expr] = "RETURN" ~> expr ^^ {
    e => e
  }

  def arguments: Parser[Map[String, Int]] = repsep(ident, ",") ^^ {
    argumentList => {
    	(for (a <- argumentList) yield (a -> 0)) toMap
    }
  }

  def block: Parser[List[Statement]] = rep(statement) ^^ { a => a }

  def statement: Parser[Statement] = positioned(variableAssignment | outStatement | ifStatement | executeFunction | outStatement) ^^ { a => a }

  def variableAssignment: Parser[VariableDefinition] = "DIM" ~> ident ~ "=" ~ positioned(executeFunction | expr) ^^ { case a ~ "=" ~ b => { new VariableDefinition(a, b) } }

  def outStatement: Parser[PrintStatement] = "PRINT" ~> positioned(expr) ^^ { case a => new PrintStatement(a) }

  def ifStatement: Parser[IfStatement] = conditional ~ block ^^ {
    case a ~ b ~ c => {
      c match {
        case None => new IfStatement(a, b, List())
        case _ => new IfStatement(a, b, c.get)
      }
    }
  }

  def conditional: Parser[Condition] = "IF" ~ "(" ~> condition <~ ")" ~ "THEN"

  def condition: Parser[Condition] = positioned(expr) ~ ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ positioned(expr) ^^ {
    case a ~ b ~ c => {
      new Condition(b, a, c)
    }
  }

  def iterations: Parser[Int] = numericLit ^^ { _ toInt }

  def executeFunction: Parser[CallFunctionMethod] = ((ident) <~ "(") ~ callFunctionMethod <~ ")" ^^ {
    case a ~ l => new CallFunctionMethod(a, l)
  }

  def functionCallArguments: Parser[Map[String, Expr]] = repsep(functionArgument, ",") ^^ {
    _ toMap
  }

  def functionArgument: Parser[(String, Expr)] = (ident <~ "=") ~ expr ^^ {
    case a ~ b => (a, b)
  }

  def expr: Parser[Expr] = term ~ rep(("+" | "-") ~ term) ^^ {
    case a ~ List() => a
    case a ~ b => {
      def appendExpression(c: Operator, p: Operator): Operator = {
        p.left = c
        p
      }

      var root: Operator = new Operator(b.head._1, a, b.head._2)

      for (f <- b.tail) {
        var parent =
          f._1 match {
            case "+" => new Operator("+", null, f._2)
            case "-" => Operator("-", null, f._2)
          }

        root = appendExpression(root, parent)
      }

      root
    }
  }

  def term: Parser[Expr] = multiplydividemodulo ^^ { l => l } | factor ^^ {
    a => a
  }

  def multiplydividemodulo: Parser[Expr] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {

    case a ~ List() => a
    case a ~ b => {
      def appendExpression(e: Operator, t: Operator): Operator = {
        t.left = e.right
        e.right = t
        t
      }

      var root: Operator = new Operator(b.head._1, a, b.head._2)
      var current = root

      // for each of these, i'm just building up the parse tree
      for (f <- b.tail) {
        var rightOperator =
          f._1 match {
            case "*" => Operator("*", null, f._2)
            case "/" => Operator("/", null, f._2)
            case "%" => Operator("%", null, f._2)
          }

        current = appendExpression(current, rightOperator)
      }

      root
    }
  }

  def factor: Parser[Expr] = numericLit ^^ { a => Number(a.toInt) } |
    "(" ~> expr <~ ")" ^^ { e => e } |
    ident ^^ { new Identifier(_) }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
}