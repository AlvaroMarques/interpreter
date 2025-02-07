package evaluator

import evaluator.objects.IntegerObject
import parser.ast.{Node, Program, Statement}
import parser.ast.expressions.IntegerLiteral
import parser.ast.statements.ExpressionStatement

import scala.annotation.tailrec

object Evaluator {

  @tailrec
  def apply(node: Node): Option[Anything] = node match {
    case node: Program => evalStatements(node.statements)
    case node: ExpressionStatement => Evaluator(node)
    case node: IntegerLiteral => Some(IntegerObject(node.value))
    case _ => None
  }

  def evalStatements(statements: Seq[Statement]): Option[Anything] = statements.map(Evaluator(_)).reduceOption((_, b) => b) match {
    case Some(objectOption) => objectOption
    case _ => None
  }

}
