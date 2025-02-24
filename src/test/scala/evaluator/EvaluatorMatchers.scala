package evaluator

import evaluator.objects.{BooleanObject, ErrorObject, IntegerObject, NullObject, StringObject}
import lexer.Lexer
import org.scalatest.matchers._
import parser.Parser

trait EvaluatorMatchers {

  def beEvaluatedWithType(objectType: ObjectType) = new TypeEvaluatorMatcher(objectType)

  def beEqualTo[T](value: T) = new ValueEvaluatorMatcher(value: T)
  def beInspectedInto(value: String) = new InspectionMatcher(expectedString = value)
  def failWithMessage(message: String) = new ErrorHandlerMatcher(message)

  class ErrorHandlerMatcher(expectedMessage: String) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val eval = Evaluator()
      val value = eval.evaluate(program, context = "__global__")

      MatchResult(
        matches = value match {
          case Some(t: ErrorObject) =>
            t.message == expectedMessage
          case _ => false
        },
        s"""'$input' should have the following error: '$expectedMessage' but instead got ${eval.error}"""
        ,
        s"The program $input returned the right type!"
      )
    }
  }

  class InspectionMatcher(expectedString: String) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val eval = Evaluator()
      val value = eval.evaluate(program, context = "__global__")
      MatchResult(
        matches = value match {
          case Some(t) => t.inspect == expectedString
          case None => false
        },
        s"""'$input' should be inspected into '$expectedString' but it's actually ${value.getOrElse(NullObject).inspect}
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

  class TypeEvaluatorMatcher(objectType: ObjectType) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val eval = Evaluator()
      val value = eval.evaluate(program, context = "__global__")
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
      val value = eval.evaluate(program, context = "__global__")
      MatchResult(
        matches = value match {
          case Some(t: BooleanObject) => valueToCheck match {
            case valueToCheck: Boolean => t.value == valueToCheck
            case _ => false
          }
          case Some(s: StringObject) => valueToCheck match {
            case valueToCheck: String => s.value == valueToCheck
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
