package evaluator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EvaluatorSpec extends AnyFlatSpec with EvaluatorMatchers {

  "Literals" should "be correctly valuated" in {
    "5" should beEvaluatedWithType(ObjectType.Integer)
    "5" should beEqualTo(BigInt(5))
    "10" should beEvaluatedWithType(ObjectType.Integer)
    "10" should beEqualTo(BigInt(10))

    "false" should beEvaluatedWithType(ObjectType.Boolean)
    "false" should beEqualTo(false)
    "true" should beEvaluatedWithType(ObjectType.Boolean)
    "true" should beEqualTo(true)

  }

  "Prefix expressions" should "be correctly valuated" in {
    "!false" should beEqualTo(true)
    "!true" should beEqualTo(false)
    "!5" should beEqualTo(false)

    "-5" should beEqualTo(BigInt(-5))
    "-232131" should beEqualTo(-232131)
    "-false" should beEqualTo(value = None)
  }

  "Infix expressions" should "be correctly valuated" in {
    "1 + 5" should beEqualTo(6)
    "50 / 2 * 2 + 10" should beEqualTo(60)
    "2 * (5 + 10)" should beEqualTo(30)
    "3 * 3 * 3 + 10" should beEqualTo(37)
    "3 * (3 * 3) + 10" should beEqualTo(37)
    "(5 + 10 * 2 + 15 / 3) * 2 + -10" should beEqualTo(50)

    "true" should beEqualTo(true)
    "false" should beEqualTo(false)
    "1 < 2" should beEqualTo(true)
    "1 > 2" should beEqualTo(false)
    "1 < 1" should beEqualTo(false)
    "1 > 1" should beEqualTo(false)
    "1 == 1" should beEqualTo(true)
    "1 != 1" should beEqualTo(false)
    "1 == 2" should beEqualTo(false)
    "1 != 2" should beEqualTo(true)

    "true == true" should beEqualTo(true)
    "false == false" should beEqualTo(true)
    "true == false" should beEqualTo(false)
    "true != false" should beEqualTo(true)
    "false != true" should beEqualTo(true)
    "(1 < 2) == true" should beEqualTo(true)
    "(1 < 2) == false" should beEqualTo(false)
    "(1 > 2) == true" should beEqualTo(false)
    "(1 > 2) == false" should beEqualTo(true)
  }

  "Conditional operators" should "be evaluated correctly" in {
    "if (true) { 10 }" should beEqualTo(10)
    "if (false) { 10 }" should beEqualTo(value = None)
    "if (1) { 10 }" should beEqualTo(10)
    "if (1 < 2) { 10 }" should beEqualTo(10)
    "if (1 > 2) { 10 }" should beEqualTo(value = None)
    "if (1 > 2) { 10 } else { 20 }" should beEqualTo(20)
    "if (1 < 2) { 10 } else { 20 }" should beEqualTo(10)
  }

  "Return statements" should "be evaluated correctly" in {
    "return 10;" should beEqualTo(10)
    "return 10; 9;" should beEqualTo(10)
    "return 2 * 5; 9;" should beEqualTo(10)
    "9; return 2 * 5; 9;" should beEqualTo(10)
    """
      |if (10 > 1) {
      |  if (10 > 1) {
      |    return 10;
      |  }
      |
      |  return 1;
      |}
      |""" should beEqualTo(10)
  }

  "Error Handling" should "work" in {
    "5 + true;" should failWithMessage("type mismatch: INTEGER + BOOLEAN")
    "5 + true; 5;" should failWithMessage("type mismatch: INTEGER + BOOLEAN")
    "-true" should failWithMessage("unknown operator: -BOOLEAN")
    "true + false;" should failWithMessage("type mismatch: BOOLEAN + BOOLEAN")
    "if (10 > 1) ( true + false; )" should failWithMessage("type mismatch: BOOLEAN + BOOLEAN")
    "5; true + false; 5" should failWithMessage("type mismatch: BOOLEAN + BOOLEAN")
    """
      |if (10 > 1) {
      |  if (10 > 1) {
      |    return true + false;
      |  }
      |
      |  return 1;
      |}
      |""".stripMargin should failWithMessage("type mismatch: BOOLEAN + BOOLEAN")

  }
}
