package parser.ast.statements

import parser.ast.{Expression, Statement}
import token.Token

case class ExpressionStatement(
    token: Token,
    expression: Option[Expression]
) extends Statement {
  override def statementNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = expression match {
    case Some(expression) => expression.string
    case None             => ""
  }
}
