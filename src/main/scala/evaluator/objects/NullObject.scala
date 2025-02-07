package evaluator.objects

import evaluator.{Anything, ObjectType}

case class NullObjectConstructor() extends Anything {
  override def objectType: ObjectType = ObjectType.Null

  override def inspect: String = "null"
}

object NullObject extends  NullObjectConstructor()
