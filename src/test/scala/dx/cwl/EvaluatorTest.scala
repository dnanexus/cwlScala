package dx.cwl

import java.io.FileInputStream
import java.nio.file.Paths

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.yaml.snakeyaml.Yaml

import scala.jdk.CollectionConverters._

class EvaluatorTest extends AnyWordSpec with Matchers {
  private val testCasesPath = Paths.get(getClass.getResource(s"/params/params_inc.yml").getPath)
  private val testCases =
    new Yaml()
      .load[java.util.List[Any]](new FileInputStream(testCasesPath.toFile))
      .asScala

  private def getString(obj: Any): String = {
    obj match {
      case s: String => s
      case _         => throw new Exception(s"expected string, not ${obj}")
    }
  }

  "expression parser" should {
    val bar = Map(
        "baz" -> StringValue("zab1"),
        "b az" -> IntValue(2),
        "b'az" -> BooleanValue.True,
        "buz" -> ArrayValue(Vector("a", "b", "c").map(StringValue(_)))
    )
    val ctx = EvaluatorContext(inputs = ObjectValue(Map("bar" -> ObjectValue(bar))))
    testCases.foreach { x =>
      val testCase = x.asInstanceOf[java.util.Map[String, Any]].asScala
      val id = getString(testCase("id"))
      s"parse ${id}" in {
        val (cwlTypes, _) = CwlType(getString(testCase("type")))
        val outputBinding =
          testCase("outputBinding").asInstanceOf[java.util.Map[String, Any]].asScala
        val expr = getString(outputBinding("outputEval"))
        Evaluator.default(expr, cwlTypes, ctx)
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
                CompoundString(Vector(StringLiteral("a $(b)${c}"), EcmaExpr("d"))))
  )

  "string splitter" should {
    splitTestCases.foreach { testCase =>
      s"parse ${testCase.s}" in {
        EcmaStringParser(testCase.s) shouldBe testCase.expected
      }
    }
  }
}
