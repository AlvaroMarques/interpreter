package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class StringLiteral(token: Token, value: String) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal.substring(1, token.literal.length - 1)

  override def string: String = s"\"${token.literal}\""
}
