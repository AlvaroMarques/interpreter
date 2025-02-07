package evaluator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EvaluatorSpec extends AnyFlatSpec with EvaluatorMatchers {

  "Integers" should "be correctly valuated" in {
    "5" should beEvaluatedWithType(ObjectType.Integer)
  }
}
