package evaluator.objects

import evaluator.{Anything, ObjectType}
import parser.ast.expressions.Identifier
import parser.ast.statements.BlockStatement

trait FunctionObject extends Anything {

  override def objectType: ObjectType = ObjectType.Function
  override def inspect: String = this.toString
}
