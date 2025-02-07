package parser.ast.expressions

import parser.ast.Expression
import token.Token

case class CallExpression(
    token: Token,
    function: Expression,
    arguments: Seq[Expression]
) extends Expression {
  override def expressionNode(): Unit = ()

  override def tokenLiteral: String = token.literal

  override def string: String = {
    s"${function.string}(${arguments.map(_.string).mkString(", ")})"
  }
}
