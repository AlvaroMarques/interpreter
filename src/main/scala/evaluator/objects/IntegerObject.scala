package evaluator.objects

import evaluator.{Anything, ObjectType}

case class IntegerObject(value: Int) extends Anything {
  override def objectType: ObjectType = ObjectType.Integer
  override def inspect: String = value.toString
}
