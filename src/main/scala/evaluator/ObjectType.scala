package evaluator

import enumeratum.EnumEntry.Uppercase
import enumeratum.{Enum, EnumEntry}
sealed trait ObjectType extends EnumEntry with Uppercase

object ObjectType extends Enum[ObjectType] {
  val values: IndexedSeq[ObjectType] = findValues

  object Integer extends ObjectType
  object Boolean extends ObjectType
  object Null extends ObjectType
}
