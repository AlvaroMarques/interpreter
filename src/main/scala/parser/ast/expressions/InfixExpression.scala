package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class InfixExpression(
    token: Token,
    operator: String,
    left: Expression,
    right: Expression
) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = s"(${left.string} $operator ${right.string})"
}
