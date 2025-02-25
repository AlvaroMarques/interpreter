package parser.ast.expressions

import parser.ast.Expression
import parser.ast.statements.BlockStatement
import token.Token

case class WhileExpression(
    token: Token,
    condition: Expression,
    consequence: BlockStatement
) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = {
    s"while ${condition.string} ${consequence.string}"
  }
}
