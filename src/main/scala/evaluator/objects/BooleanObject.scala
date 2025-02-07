package evaluator.objects

import evaluator.{Anything, ObjectType}

case class BooleanObject(value: Boolean) extends Anything {
  override def objectType: ObjectType = ObjectType.Boolean
  override def inspect: String = value.toString
}
