package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class IntegerLiteral(token: Token, value: BigInt) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = s"${token.literal}"

  override def string: String = s"${token.literal}"
}
