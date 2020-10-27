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
    testCases.foreach { x =>
      val testCase = x.asInstanceOf[java.util.Map[String, Any]].asScala
      val id = getString(testCase("id"))
      s"parse ${id}" in {
        val outputBinding =
          testCase("outputBinding").asInstanceOf[java.util.Map[String, Any]].asScala
        val expr = getString(outputBinding("outputEval"))
        val cwlExpr = Evaluator.default(expr) match {
          case expr: CompoundString     => expr
          case expr: ParameterReference => expr
          case other =>
            throw new Exception(
                s"failed to correctly parse ${expr} - expected a CompoundString or ParameterReference, got ${other}"
            )
        }
        println(cwlExpr)
      }
    }
  }
}
