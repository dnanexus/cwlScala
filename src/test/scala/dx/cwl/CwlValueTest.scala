package dx.cwl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CwlValueTest extends AnyFlatSpec with Matchers {
  it should "create values from literals" in {
    val sFloat = FloatValue(0.0)
    sFloat.value shouldBe 0.0
    val jFloat = FloatValue(new java.lang.Double(0.0))
    jFloat.value shouldBe 0.0
  }
}
