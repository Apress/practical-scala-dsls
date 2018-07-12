package practical.dsl.parsers.runtime
import practical.dsl.parsers.model._

class Translator(program: Program) {
  var currentScope = new Scope("global", null)

  def run() {
    walk(program.statements)
  }

  private def getVariable(ident: Identifier): Expr = {
    var s: Scope = currentScope

    while ((!s.name.equals("global")) && !s.variables.contains(ident.name)) s = s.parentScope

    if (s.variables.contains(ident.name)) s.variables(ident.name)
    else {
      sys.error("Error: Undefined variable " + ident.name )
    }
  }

  private def calculateExpr(e: Expr): Int = {
    e match {
      case Number(value) => value
      case Identifier(name) => {
        calculateExpr(getVariable(e.asInstanceOf[Identifier]))
      }
      case Operator(op, left, right) => {
        op match {
          case "*" => calculateExpr(left) * calculateExpr(right)
          case "/" => calculateExpr(left) / calculateExpr(right)
          case "%" => calculateExpr(left) % calculateExpr(right)
          case "+" => calculateExpr(left) + calculateExpr(right)
          case "-" => calculateExpr(left) - calculateExpr(right)
        }
      }
    }
  }

  private def isConditionTrue(condition: Condition): Boolean = {
    val a = calculateExpr(condition.left)
    val b = calculateExpr(condition.right)

    condition.op match {
      case "==" => (a == b)
      case "!=" => (a != b)
      case "<=" => (a <= b)
      case "<" => (a < b)
      case ">=" => (a >= b)
      case ">" => (a > b)
    }
  }

  private def executeFunction(f: Function, arguments: Map[String, Expr]) {
    currentScope = new Scope(f.name, currentScope)

    for (v <- arguments) currentScope.variables(v._1) = v._2

    walk(f.statements)

    currentScope = currentScope.parentScope
  }

  private def walk(tree: List[Statement]) {
    if (!tree.isEmpty) {
      tree.head match {
        case FunctionCall(name, values) => {
          val f = program.functions.filter(x => x.name == name)

          if (f.size < 1) sys.error("Error: Undefined function '" + name + "')
          else {
            executeFunction(f(0), values)

            walk(tree.tail)
          }
        }
        case Variable(name, value) => {
          if (value.isInstanceOf[FunctionCall]) {
            val functionCall = value.asInstanceOf[FunctionCall]
            val function = program.functions.filter(x => x.name == functionCall.name)

            if (function.size < 1) sys.error("Error: Undefined function '" + functionCall.name + ")
            else {
              executeFunction(function(0), functionCall.values)

              currentScope = currentScope.parentScope

              currentScope.variables(name) = function(0).returnValue
            }
          } else {
            currentScope.variables(name) = value
          }

          walk(tree.tail)
        }
        case PrintStatement(value) => {
          println(calculateExpr(value))
          walk(tree.tail)
        }
        case IfStatement(condition, trueBranch, falseBranch) => {
          currentScope = new Scope("", currentScope)

          if (isConditionTrue(condition)) walk(trueBranch) else walk(falseBranch)

          currentScope = currentScope.parentScope

          walk(tree.tail)
        }
        case _ => ()
      }
    }
  }
}