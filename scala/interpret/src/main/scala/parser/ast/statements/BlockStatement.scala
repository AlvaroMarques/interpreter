package parser.ast.statements

import com.typesafe.scalalogging.Logger
import parser.ast.{Expression, Statement}
import token.Token

case class BlockStatement(
    token: Token,
    statements: Seq[Statement]
) extends Statement {
  override def statementNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  override def string: String = {
    s"{ ${statements.map(_.string).mkString(";")} }"
  }
}
