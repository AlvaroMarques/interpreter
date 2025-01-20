package parser.ast.expressions

import enumeratum._
import enumeratum.EnumEntry._

sealed trait ExpressionOrdering extends EnumEntry with Uppercase {
  def >(right: ExpressionOrdering): Boolean = {
    ExpressionOrdering.valuesToIndex(this) > ExpressionOrdering.valuesToIndex(
      right
    )
  }
  def <(right: ExpressionOrdering): Boolean = {
    ExpressionOrdering.valuesToIndex(this) < ExpressionOrdering.valuesToIndex(
      right
    )
  }
}

object ExpressionOrdering extends Enum[ExpressionOrdering] {

  val values: IndexedSeq[ExpressionOrdering] = findValues

  case object Lowest extends ExpressionOrdering
  case object Equal extends ExpressionOrdering
  case object LessGreater extends ExpressionOrdering
  case object Sum extends ExpressionOrdering
  case object Product extends ExpressionOrdering
  case object Prefix extends ExpressionOrdering
  case object Call extends ExpressionOrdering

}
