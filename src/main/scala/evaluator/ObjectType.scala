package evaluator

import enumeratum.{Enum, EnumEntry}

sealed trait ObjectType extends EnumEntry {
  /* Making Object type string with uppercase  */
  override def toString: String = this.getClass.getSimpleName.replace("$", "").toUpperCase
}

object ObjectType extends Enum[ObjectType] {
  val values: IndexedSeq[ObjectType] = findValues

  object Integer extends ObjectType

  object String extends ObjectType

  object Boolean extends ObjectType

  object Error extends ObjectType

  object Null extends ObjectType

  object Function extends ObjectType

  object Array extends ObjectType
}
