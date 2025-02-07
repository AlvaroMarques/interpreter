package parser.ast.expressions

import parser.ast.Expression
import parser.ast.statements.BlockStatement
import token.Token

case class FunctionLiteral(
    token: Token,
    parameters: Seq[Identifier],
    body: BlockStatement
) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String =
    s"fn (${parameters.map(_.string).mkString(", ")}) ${body.string} "
}
