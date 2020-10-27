package dx.js

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.DefaultJsonProtocol._

class JsTest extends AnyFlatSpec with Matchers {
  private val runtime = Runtime()

  it should "evaluate a javascript file" in {
    val path = Paths.get(getClass.getResource(s"/js/test-functions.js").getPath)
    val result = runtime.evalFile[Double](path)
    result shouldBe None
  }
}
