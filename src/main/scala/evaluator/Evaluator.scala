package evaluator

import com.typesafe.scalalogging.Logger
import evaluator.objects.{BooleanObject, IntegerObject}
import parser.ast.{Expression, Node, Program, Statement}
import parser.ast.expressions.{BooleanLiteral, IntegerLiteral, PrefixExpression}
import parser.ast.statements.ExpressionStatement

import scala.annotation.tailrec

object Evaluator {

  val logger: Logger = Logger(Evaluator.getClass)

  @tailrec
  def apply(node: Node): Option[Anything] = {
    node match {
      case node: Program => evalStatements(node.statements)
      case node: ExpressionStatement => node.expression match {
        case Some(expression) => Evaluator(expression)
        case _ => None
      }
      case node: IntegerLiteral => Some(IntegerObject(node.value))
      case node: BooleanLiteral => Some(BooleanObject.get(node.value))
      case node: PrefixExpression =>
        val right = node.right
        evalPrefixExpression(node.operator, right)
      case _ => None
    }
  }

  def evalPrefixExpression(operator: String, expression: Expression): Option[Anything] = {
    None
  }

  def evalStatements(statements: Seq[Statement]): Option[Anything] = {
    statements.map(Evaluator(_)).reduceOption((_, b) => b) match {
      case Some(objectOption) => objectOption
      case _ => None
    }
  }

}
