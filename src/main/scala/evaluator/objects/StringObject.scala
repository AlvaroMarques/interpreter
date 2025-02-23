package evaluator.objects

import evaluator.{Anything, ObjectType}

case class StringObject(value: String) extends Anything {
  override def objectType: ObjectType = ObjectType.String
  override def inspect: String = value
}
