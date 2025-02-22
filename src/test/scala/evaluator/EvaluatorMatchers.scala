package evaluator

import evaluator.objects.{BooleanObject, IntegerObject, NullObject}
import lexer.Lexer
import org.scalatest.matchers._
import parser.Parser

trait EvaluatorMatchers {

  def beEvaluatedWithType(objectType: ObjectType) = new TypeEvaluatorMatcher(objectType)

  def beEqualTo[T](value: T) = new ValueEvaluatorMatcher(value: T)

  class ErrorHandlerMatcher(expectedMessage: String) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val eval = Evaluator()
        val value = eval.evaluate(program)

      MatchResult(
        matches = eval.error match {
          case Some(t) => t.message == expectedMessage
          case None => false
        },
        s"""'$input' should have the following error: '$expectedMessage' but instead got ${eval.error}"""
        ,
        s"The program $input returned the right type!"
      )
    }
  }

  class TypeEvaluatorMatcher(objectType: ObjectType) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val eval = Evaluator()
      val value = eval.evaluate(program)
      MatchResult(
        matches = value match {
          case Some(t) => t.objectType == objectType
          case None => false
        },
        s"""'$input' be evaluated for type ${objectType.toString} but instead became ${value.getOrElse(NullObject).objectType}
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

  class ValueEvaluatorMatcher[T](valueToCheck: T) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val eval = Evaluator()
      val value = eval.evaluate(program)
      MatchResult(
        matches = value match {
          case Some(t: BooleanObject) => valueToCheck match {
            case valueToCheck: Boolean => t.value == valueToCheck
            case _ => false
          }
          case Some(NullObject) => valueToCheck match {
            case valueToCheck: Option[_] if valueToCheck.isEmpty => true
            case _ => false
          }
          case Some(t: IntegerObject) => valueToCheck match {
            case valueToCheck: BigInt => t.value == valueToCheck
            case valueToCheck: Int => t.value == BigInt(valueToCheck)
            case valueToCheck: Long => t.value == BigInt(valueToCheck)
            case _ => false
          }
          case None => false
        },
        s"""'$input' should be equal to $valueToCheck, but its not, obj returned is $value"
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
}
