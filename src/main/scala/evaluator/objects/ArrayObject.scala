package evaluator.objects

import evaluator.{Anything, ObjectType}
import parser.ast.expressions.Identifier
import parser.ast.statements.BlockStatement

case class ArrayObject(values: Seq[Anything]) extends Anything {
  override def objectType: ObjectType = ObjectType.Array
  override def inspect: String = s"[${values.map(_.inspect).mkString(",")}]"

  def isEqual(right: ArrayObject): Boolean = {
      right.inspect == inspect
  }
}
