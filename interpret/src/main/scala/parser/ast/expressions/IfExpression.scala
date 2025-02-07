package parser.ast.expressions

import parser.ast.Expression
import parser.ast.statements.BlockStatement
import token.Token

case class IfExpression(
    token: Token,
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option[BlockStatement] = None
) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = {
    s"if ${condition.string}  ${consequence.string}" + {
      alternative match {
        case Some(alternative) => s" else ${alternative.string}"
        case None              => ""
      }
    }
  }
}
