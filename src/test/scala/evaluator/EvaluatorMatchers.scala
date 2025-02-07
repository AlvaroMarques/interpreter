package evaluator

import lexer.Lexer
import org.scalatest.matchers._
import parser.Parser

trait EvaluatorMatchers {

  class TypeEvaluatorMatcher(objectType: ObjectType) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val value = Evaluator.evaluator(program)
      MatchResult(
        matches = value.objectType == objectType,
        s"""'$input' be evaluated for type ${objectType.toString} but instead became ${value.objectType}
           | also, the following errors were found when parsing the code: ${
          parser.errors
            .map(_.message)
            .mkString("\n\t")
        }
           |""".stripMargin,
        s"The program $input returned the right type!"
      )
    }
  }

  def beEvaluatedWithType(objectType: ObjectType) = new TypeEvaluatorMatcher(objectType)
}
