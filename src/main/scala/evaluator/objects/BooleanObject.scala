package evaluator.objects

import evaluator.{Anything, ObjectType}

case class BooleanObject(value: Boolean) extends Anything {
  override def objectType: ObjectType = ObjectType.Boolean
  override def inspect: String = value.toString
}

object BooleanObject {
  val False: BooleanObject = BooleanObject(false)
  val True: BooleanObject = BooleanObject(true)
  def get(value: Boolean): BooleanObject =
    if (value) {
      True
    } else {
      False
    }
}
