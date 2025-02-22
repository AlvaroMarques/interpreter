package evaluator.objects

import evaluator.{Anything, ObjectType}

case class ErrorObject(message: String) extends Anything {
  override def objectType: ObjectType = ObjectType.Error

  override def inspect: String = s"error: $message"
}
