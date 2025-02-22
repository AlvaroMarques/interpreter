package evaluator

import com.typesafe.scalalogging.Logger
import evaluator.objects.BooleanObject.{False, True}
import evaluator.objects.{BooleanObject, ErrorObject, IntegerObject, NullObject, NullObjectConstructor, ReturnValue}
import parser.ast.expressions.{BooleanLiteral, IfExpression, InfixExpression, IntegerLiteral, PrefixExpression}
import parser.ast.statements.{BlockStatement, ExpressionStatement, ReturnStatement}
import parser.ast.{Node, Program, Statement}

case class Evaluator() {

  val logger: Logger = Logger(Evaluator.getClass)
  var error: Option[ErrorObject] = None

  def evaluate(node: Node): Option[Anything] = {
    node match {
      case node: Program => evalStatements(node.statements)
      case node: ExpressionStatement => node.expression match {
        case Some(expression) => evaluate(expression)
        case _ => None
      }
      case node: BlockStatement => evalBlockStatement(node)
      case node: IntegerLiteral => Some(IntegerObject(node.value))
      case node: BooleanLiteral => Some(BooleanObject.get(node.value))
      case node: InfixExpression =>
        (evaluate(node.left), evaluate(node.right))match {
          case (Some(left), Some(right)) =>
            evalInfixExpression(node.operator, left, right)
          case _ => None
        }
      case node: PrefixExpression =>
        evaluate(node.right) match {
          case Some(right) =>
            evalPrefixExpression(node.operator, right)
          case _ => None
        }
      case node: IfExpression =>
        evalIfExpression(node)
      case ReturnStatement(_, returnValue) =>
        evaluate(returnValue) match {
          case Some(value) => Some(ReturnValue(value))
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

  def evalIfExpression(expression: IfExpression): Option[Anything] = {
    evaluate(expression.condition) match {
      case Some(condition: BooleanObject) =>
        if (condition.value) {
          evaluate(expression.consequence)
        } else {
          expression.alternative match {
            case Some(alternative) => evaluate(alternative)
            case _ => Some(NullObject)
          }
        }
      case Some(_: NullObjectConstructor) =>
        expression.alternative match {
          case Some(alternative) => evaluate(alternative)
          case _ => Some(NullObject)
        }
      case Some(value: IntegerObject) if value.value == 0 =>
        expression.alternative match {
          case Some(alternative) => evaluate(alternative)
          case _ => Some(NullObject)
        }
      case Some(_) =>
        evaluate(expression.consequence)
      case _ => None
    }
  }

  def evalMinusOperator(expression: Anything): Option[Anything] = {
    expression match {
      case IntegerObject(value) => Some(IntegerObject(-value))
      case otherObject =>
        error = Some(ErrorObject(s"unknown operator: -${otherObject.objectType.toString}"))
        Some(NullObject)
    }
  }

  def evalIntegerInfixExpression(operator: String, left: IntegerObject, right: IntegerObject): Option[Anything] = {
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

  def evalInfixExpression(operator: String, left: Anything, right: Anything): Option[Anything] = {
    (left, right) match {
      case (left: IntegerObject, right: IntegerObject) => evalIntegerInfixExpression(operator, left, right)
      case _ => operator match {
        case "==" => Some(BooleanObject(value = left == right))
        case "!=" => Some(BooleanObject(value = left != right))
        case operator: String =>
          error = Some(ErrorObject(s"type mismatch: ${left.objectType.toString} $operator ${right.objectType}"))
          None
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

  def evalBlockStatement(blockStatement: BlockStatement): Option[Anything] = {
    val statementsEvaluations = blockStatement.statements.map(evaluate)

    if (statementsEvaluations.isEmpty) {
      None
    } else {
      logger.info(s"$statementsEvaluations")
      statementsEvaluations
        .reduce(
          (a, b) => (a, b) match {
            case (Some(rv: ReturnValue), _) => Some(rv)
            case (_, Some(b)) => Some(b)
            case _ => None
          }
        )
    }

  }

  def evalStatements(statements: Seq[Statement]): Option[Anything] = {
    val statementsEvaluations = statements.map(evaluate)

    if (statementsEvaluations.isEmpty) {
      None
    } else {
      logger.info(s"$statementsEvaluations")
      statementsEvaluations
        .reduce(
          (a, b) => (a, b) match {
            case (Some(rv: ReturnValue), Some(_)) => Some(rv)
            case (_, Some(b)) => Some(b)
            case _ => None
          }
        ) match {
          case Some(returnValue: ReturnValue) => Some(returnValue.value)
          case other => other
        }
    }
  }

}
