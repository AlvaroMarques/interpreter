package evaluator.objects

import evaluator.{Anything, ObjectType}
import parser.ast.expressions.Identifier
import parser.ast.statements.BlockStatement

case class CustomFunctionObject(parameters: Seq[Identifier], body: BlockStatement) extends FunctionObject {
  override def objectType: ObjectType = ObjectType.Function
  override def inspect: String = this.toString
}
