package dx.cwl

import java.io.FileInputStream
import java.nio.file.{Files, Path, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.yaml.snakeyaml.Yaml

import scala.collection.immutable.SeqMap
import scala.jdk.CollectionConverters._

class EvaluatorTest extends AnyWordSpec with Matchers {
  private def getPath(resource: String): Path = {
    Paths.get(getClass.getResource(resource).getPath)
  }

  private def loadYamlTestCases(resource: String): Vector[Any] = {
    new Yaml()
      .load[java.util.List[Any]](new FileInputStream(getPath(resource).toFile))
      .asScala
      .toVector
  }

  private def getString(obj: Any): String = {
    obj match {
      case s: String => s
      case _         => throw new Exception(s"expected string, not ${obj}")
    }
  }

  private val bar = SeqMap(
      "baz" -> StringValue("zab1"),
      "b az" -> IntValue(2),
      "b'az" -> BooleanValue.True,
      "buz" -> ArrayValue(Vector("a", "b", "c").map(StringValue(_))),
      "bork" -> IntValue(1)
  )

  private val ctx = EvaluatorContext(inputs = ObjectValue(SeqMap("bar" -> ObjectValue(bar))))
  private val trace = false

  "parameter reference evaluator" should {
    val testCases = loadYamlTestCases(s"/tools/pass/params_inc.yml")
    val evaluator = Evaluator(trace = trace)
    testCases.foreach { x =>
      val testCase = x.asInstanceOf[java.util.Map[String, Any]].asScala
      val id = getString(testCase("id"))
      s"evaluate ${id}" in {
        val (cwlTypes, _) = CwlType(getString(testCase("type")))
        val outputBinding =
          testCase("outputBinding").asInstanceOf[java.util.Map[String, Any]].asScala
        val expr = getString(outputBinding("outputEval"))
        evaluator(expr, cwlTypes, ctx)
      }
    }
  }

  private case class SplitTest(s: String, expected: EcmaString)

  private val splitTestCases: Vector[SplitTest] = Vector(
      SplitTest("a $(b) c ${d}",
                CompoundString(
                    Vector(StringLiteral("a "),
                           EcmaExpr("b"),
                           StringLiteral(" c "),
                           EcmaFunctionBody("d"))
                )),
      SplitTest("a \\$(b)\\${c}$(d)",
                CompoundString(Vector(StringLiteral("a $(b)${c}"), EcmaExpr("d")))),
      SplitTest("$(add2(inputs.bork))", EcmaExpr("add2(inputs.bork)"))
  )

  "string splitter" should {
    val parser = EcmaStringParser(trace = trace)
    splitTestCases.foreach { testCase =>
      s"parse ${testCase.s}" in {
        parser(testCase.s) shouldBe testCase.expected
      }
    }
  }

  "javascript evaluator" should {
    val lib = new String(Files.readAllBytes(getPath("/js/test-functions.js")))
    val evaluator = Evaluator(jsEnabled = true, jsLibrary = Some(lib), trace = trace)
    val testCases = loadYamlTestCases("/js/expressions.yml")
    testCases.foreach { x =>
      val testCase = x.asInstanceOf[java.util.Map[String, Any]].asScala
      val id = getString(testCase("id"))
      s"evaluate ${id}" in {
        val (cwlTypes, _) = CwlType(getString(testCase("type")))
        val outputBinding =
          testCase("outputBinding").asInstanceOf[java.util.Map[String, Any]].asScala
        val expr = getString(outputBinding("outputEval"))
        evaluator(expr, cwlTypes, ctx)
      }
    }
  }
}
