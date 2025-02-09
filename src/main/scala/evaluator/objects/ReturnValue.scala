package evaluator.objects

import evaluator.{Anything, ObjectType}

case class ReturnValue(value: Anything) extends Anything {

  override def objectType: ObjectType = value.objectType

  override def inspect: String = value.inspect
}
