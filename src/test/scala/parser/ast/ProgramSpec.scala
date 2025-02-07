package parser.ast

import org.scalatest.flatspec.AnyFlatSpec
import parser.ast.expressions.Identifier
import parser.ast.statements.LetStatement
import token.{Token, TokenType}

class ProgramSpec extends AnyFlatSpec {
  "The string attribute" should "output the full program by its statements" in {
    val program = Program(
      Seq(
        LetStatement(
          Token(TokenType.LET, "let"),
          Identifier(Token(TokenType.IDENT, "myVar"), "myVar"),
          Identifier(Token(TokenType.IDENT, "anotherVar"), "anotherVar")
        )
      )
    )

    assert(program.string == "let myVar = anotherVar")
  }

}
