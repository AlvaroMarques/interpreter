package evaluator

import com.typesafe.scalalogging.Logger
import evaluator.objects.BooleanObject.{False, True}
import evaluator.objects.{BooleanObject, IntegerObject, NullObject}
import parser.ast.expressions.{BooleanLiteral, InfixExpression, IntegerLiteral, PrefixExpression}
import parser.ast.statements.ExpressionStatement
import parser.ast.{Node, Program, Statement}

object Evaluator {

  val logger: Logger = Logger(Evaluator.getClass)

  def apply(node: Node): Option[Anything] = {
    node match {
      case node: Program => evalStatements(node.statements)
      case node: ExpressionStatement => node.expression match {
        case Some(expression) => Evaluator(expression)
        case _ => None
      }
      case node: IntegerLiteral => Some(IntegerObject(node.value))
      case node: BooleanLiteral => Some(BooleanObject.get(node.value))
      case node: InfixExpression =>
        (Evaluator(node.left), Evaluator(node.right) ) match {
          case (Some(left), Some(right)) =>
            evalInfixExpression(node.operator, left, right)
          case _ => None
        }
      case node: PrefixExpression =>
        Evaluator(node.right) match {
          case Some(right) =>
            evalPrefixExpression(node.operator, right)
          case _ => None
        }

      case _ => None
    }
  }

  def evalBangOperator(expression: Anything): Option[BooleanObject] = {
    expression match {
      case BooleanObject(value: Boolean) => Some(BooleanObject(!value))
      case IntegerObject(value) if value == 0 => Some(True)
      case IntegerObject(_) => Some(False)
      case _ => Some(False)
    }
  }

  def evalMinusOperator(expression: Anything): Option[Anything] = {
    expression match {
      case IntegerObject(value) => Some(IntegerObject(-value))
      case _ => Some(NullObject)
    }
  }

  def evalIntegerInfixExpression(operator: String, left: IntegerObject,  right: IntegerObject): Option[Anything] = {
    operator match {
      case "+" => Some(IntegerObject(value = left.value + right.value))
      case "-" => Some(IntegerObject(value = left.value - right.value))
      case "/" => Some(IntegerObject(value = left.value / right.value))
      case "*" => Some(IntegerObject(value = left.value * right.value))
      case "<" => Some(BooleanObject(value = left.value < right.value))
      case ">" => Some(BooleanObject(value = left.value > right.value))
      case "==" => Some(BooleanObject(value = left.value == right.value))
      case "!=" => Some(BooleanObject(value = left.value != right.value))
      case _ => Some(NullObject)
    }
  }

  def evalInfixExpression(operator: String, left: Anything,  right: Anything): Option[Anything] = {
    (left, right) match {
      case (left: IntegerObject, right: IntegerObject) => evalIntegerInfixExpression(operator, left, right)
      case _ => operator match {
        case "==" => Some(BooleanObject(value = left == right))
        case "!=" => Some(BooleanObject(value = left != right))
      }
    }
  }

  def evalPrefixExpression(operator: String, right: Anything): Option[Anything] = {
    operator match {
      case "!" => evalBangOperator(right)
      case "-" => evalMinusOperator(right)
      case _ => None
    }
  }

  def evalStatements(statements: Seq[Statement]): Option[Anything] = {
    statements.map(Evaluator(_)).reduceOption((_, b) => b) match {
      case Some(objectOption) => objectOption
      case _ => None
    }
  }

}
