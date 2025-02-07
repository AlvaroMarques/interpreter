package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class PrefixExpression(token: Token, operator: String, right: Expression)
    extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = s"($operator${right.string})"
}
