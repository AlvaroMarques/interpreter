package evaluator.objects

import evaluator.{Anything, ObjectType}
import parser.ast.expressions.Identifier
import parser.ast.statements.BlockStatement

trait BuiltinFunctionObject extends FunctionObject  {
  val name: String
  def executor: Seq[Anything] => Option[Anything]
}
