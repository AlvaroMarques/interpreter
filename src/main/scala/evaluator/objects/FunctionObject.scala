package evaluator.objects

import evaluator.{Anything, ObjectType}
import parser.ast.expressions.Identifier
import parser.ast.statements.BlockStatement

case class FunctionObject(parameters: Seq[Identifier], body: BlockStatement) extends Anything {
  override def objectType: ObjectType = ObjectType.Function
  override def inspect: String = this.toString
}
