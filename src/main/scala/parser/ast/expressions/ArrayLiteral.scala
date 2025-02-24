package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class ArrayLiteral(
    token: Token,
    values: Seq[Expression]
) extends Expression {
  override def expressionNode(): Unit = ()

  override def tokenLiteral: String = token.literal

  override def string: String = {
    s"[${values.map(_.string).mkString(", ")}]"
  }
}
