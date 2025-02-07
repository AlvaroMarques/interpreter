package evaluator

import evaluator.Object.ObjectType

object Object {
  type ObjectType = String
}

trait Object {
  def objectType: ObjectType
  def inspect: String

}
