package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class Identifier(
    token: Token,
    value: String
) extends Expression {
  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = value
}
