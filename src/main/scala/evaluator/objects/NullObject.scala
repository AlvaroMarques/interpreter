package evaluator.objects

import evaluator.{Anything, ObjectType}

case class NullObject() extends Anything {
  override def objectType: ObjectType = ObjectType.Null

  override def inspect: String = "null"
}
