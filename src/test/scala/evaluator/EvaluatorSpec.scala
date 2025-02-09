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
}
