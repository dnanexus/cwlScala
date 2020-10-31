package dx.js

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json._
import spray.json.DefaultJsonProtocol._

class JsTest extends AnyFlatSpec with Matchers {
  private val engine = Engine()

  it should "evaluate a javascript file" in {
    val path = Paths.get(getClass.getResource(s"/js/test-script.js").getPath)
    val result = engine.evalFile[Double](path)
    result shouldBe Some(6.0)
  }

  it should "evaluate javascript to JSON" in {
    val result =
      engine.evalToJson(s"""var add = function(a, b) {
                           |  return a + b;
                           |}
                           |
                           |var add2 = function(a) {
                           |  return add(a, 2);
                           |}
                           |
                           |add(add2(1), 3);""".stripMargin)
    result shouldBe Some(JsNumber(6))
  }
}
