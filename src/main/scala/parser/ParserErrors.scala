package parser

import errors.ParserError
import token.TokenType

trait ParserErrors { parser: Parser =>
  var errors: Seq[ParserError] = Seq.empty[ParserError]

  def peekError(tokenType: TokenType): Unit = {
    errors ++= Seq(
      ParserError(
        s"Expected token to be of type ${tokenType.toString}, got $pToken instead"
      )
    )
  }
}
