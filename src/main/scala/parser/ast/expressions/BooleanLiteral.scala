package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class BooleanLiteral(token: Token, value: Boolean) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = s"${token.literal}"

  override def string: String = s"${token.literal}"
}
