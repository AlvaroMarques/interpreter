package evaluator

import evaluator.objects.{BooleanObject, IntegerObject, NullObject}
import lexer.Lexer
import org.scalatest.matchers._
import parser.Parser

trait EvaluatorMatchers {

  class TypeEvaluatorMatcher(objectType: ObjectType) extends Matcher[String] {
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val value = Evaluator(program)
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

  class ValueEvaluatorMatcher[T](valueToCheck: T) extends Matcher[String]{
    def apply(input: String): MatchResult = {
      val lexer = Lexer(input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      val value = Evaluator(program)
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
            case _ => false
          }
          case None => false
        },
        s"""'$input' should be equal to $valueToCheck, but its not"
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
  def beEqualTo[T](value: T) = new ValueEvaluatorMatcher(value: T)
}
