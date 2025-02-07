package evaluator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EvaluatorSpec extends AnyFlatSpec with EvaluatorMatchers {

  "Integers" should "be correctly valuated" in {
    "5" should beEvaluatedWithType(ObjectType.Integer)
    "5" should beEqualTo(BigInt(5))
    "10" should beEvaluatedWithType(ObjectType.Integer)
    "10" should beEqualTo(BigInt(10))
  }
}
