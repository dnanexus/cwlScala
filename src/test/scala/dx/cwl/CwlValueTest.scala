package dx.cwl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json._

class CwlValueTest extends AnyFlatSpec with Matchers {
  it should "create values from literals" in {
    val sFloat = FloatValue(0.0)
    sFloat.value shouldBe 0.0
    val jFloat = FloatValue(java.lang.Double.valueOf(0.0))
    jFloat.value shouldBe 0.0
  }

  it should "deserialize a JSON array" in {
    val (_, actual) =
      CwlValue.deserialize(JsArray(Vector(JsString("hello"), JsString("goodbye"))),
                           CwlArray(CwlString, None, None, None, None),
                           Map.empty)
    val expected = ArrayValue(Vector(StringValue("hello"), StringValue("goodbye")))
    actual shouldBe expected
  }

  it should "deserialize to one of multiple types" in {
    val (t, v) = CwlValue.deserialize(JsNumber(5), CwlMulti(Vector(CwlInt, CwlBoolean)), Map.empty)
    t shouldBe CwlInt
    v shouldBe IntValue(5)
  }

  it should "deserialize the input JSON to File " in {
    val inputJson = JsObject(
        fields = Map(
            "contents" -> JsString("test"),
            "metadata" -> JsObject(
                fields = Map(
                    "int_meta" -> JsNumber("1"),
                    "str_meta" -> JsString("test"),
                    "bool_meta" -> JsBoolean(true),
                    "arr_meta" -> JsArray(
                        Vector(JsString("elem1"), JsString("elem2"), JsString("elem3"))
                    )
                )
            )
        )
    )

    val (t, v) = CwlValue.deserialize(inputJson, CwlFile, Map.empty)
    t shouldBe CwlFile
    v shouldBe FileValue(contents = Some("test"),
                         metadata = inputJson.fields.get("metadata").map(_.prettyPrint))
  }

  it should "coerce null to optional" in {
    val t = CwlOptional(CwlString)
    NullValue.coercibleTo(t) shouldBe true
    NullValue.coerceTo(t) shouldBe (t, NullValue)
  }
}
