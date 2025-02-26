package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class StringLiteral(token: Token) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal
  def value: String = tokenLiteral.substring(1, token.literal.length - 1)

  override def string: String = s"""\"${token.literal}\""""
}
