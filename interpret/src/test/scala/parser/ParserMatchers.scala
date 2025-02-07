package parser

import lexer.Lexer
import org.scalatest.matchers._

trait ParserMatchers {

  def beParsedCorrectly: Matcher[String] = { (input: String) =>
    {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      MatchResult(
        matches = program.string == input && parser.errors.isEmpty,
        s"""'$input' should be correctly parsed, but instead the Parser returned this: '${program.string}'
           | also, the following errors were found: ${parser.errors
          .map(_.message)
          .mkString("\n\t")}
           |""".stripMargin,
        s"The program $input was correctly parsed!"
      )
    }
  }
}
