package parser.ast.statements

import parser.ast.Statement
import token.{Token, TokenType}

case class BagugaStatement(
    token: Token,
    statements: Seq[Statement]
) extends Statement {
  override def statementNode(): Unit = {}

  override def tokenLiteral: String = token.literal

  /*
  def statements: Seq[Statement] = {
    nonTreatedStatements.flatMap {
      case b: BagugaStatement => b.statements
      case s: Statement       => Seq(s)
    }
  }*/

  override def string: String = {
    statements
      .filter {
        case statement: BagugaStatement => false
        case statement: Statement       => true
      }
      .map(_.string)
      .mkString(";") ++
      s", mas antes ${TokenType.POINTUP}" ++
      statements
        .collect {
          case statement: BagugaStatement =>
            statement.string
        }
        .reverse
        .mkString(
          s", mas antes ${TokenType.POINTUP}"
        )
  }
}
