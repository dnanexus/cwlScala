package dx.cwl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.{JsArray, JsString}

class CwlValueTest extends AnyFlatSpec with Matchers {
  it should "create values from literals" in {
    val sFloat = FloatValue(0.0)
    sFloat.value shouldBe 0.0
    val jFloat = FloatValue(new java.lang.Double(0.0))
    jFloat.value shouldBe 0.0
  }

  it should "deserialize a JSON array" in {
    val (_, actual) =
      CwlValue.deserialize(JsArray(Vector(JsString("hello"), JsString("goodbye"))),
                           Vector(CwlArray(Vector(CwlString), None, None, None, None)),
                           Map.empty)
    val expected = ArrayValue(Vector(StringValue("hello"), StringValue("goodbye")))
    actual shouldBe expected
  }
}
