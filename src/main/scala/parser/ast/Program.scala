package parser.ast

case class Program(statements: Seq[Statement]) extends Node {
  override def tokenLiteral: String = {
    statements.headOption match {
      case Some(statement: Statement) => statement.tokenLiteral
      case None                       => ""
    }
  }

  override def string: String = {
    statements.map(_.string).mkString
  }
}
