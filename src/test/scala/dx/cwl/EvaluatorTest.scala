package dx.cwl

import java.io.FileInputStream
import java.nio.file.{Files, Path, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}

import scala.collection.immutable.SeqMap
import scala.jdk.CollectionConverters._

class EvaluatorTest extends AnyWordSpec with Matchers {
  private def getPath(resource: String): Path = {
    Paths.get(getClass.getResource(resource).getPath)
  }

  private def loadYamlTestCases(resource: String): Vector[Any] = {
    val yamlLoader = new Load(LoadSettings.builder().build())
    yamlLoader
      .loadFromInputStream(new FileInputStream(getPath(resource).toFile))
      .asInstanceOf[java.util.List[Any]]
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
    val testCases = loadYamlTestCases(s"/tools/v1.2/params_inc.yml")
    val evaluator = Evaluator(trace = trace)
    testCases.foreach { x =>
      val testCase = x.asInstanceOf[java.util.Map[String, Any]].asScala
      val id = getString(testCase("id"))
      s"evaluate ${id}" in {
        val (cwlType, _) = CwlType.translate(getString(testCase("type")))
        val outputBinding =
          testCase("outputBinding").asInstanceOf[java.util.Map[String, Any]].asScala
        val expr = getString(outputBinding("outputEval"))
        evaluator(expr, cwlType, ctx)
      }
    }

    "evaluate input details" in {
      val tmpdir = Files.createTempDirectory("test").toRealPath()
      val f = tmpdir.resolve("test.txt")
      Files.write(f, "test".getBytes())
      val d = tmpdir.resolve("dir")
      Files.createDirectories(d)
      val f2 = d.resolve("test2.txt")
      Files.write(f2, "test2".getBytes())
      tmpdir.toFile.deleteOnExit()
      val ctx2 = EvaluatorContext(inputs = EvaluatorContext.createInputs(
          Map(
              CommandInputParameter(
                  Some(Identifier(namespace = None, frag = Some("f"))),
                  None,
                  None,
                  CwlFile,
                  None,
                  None,
                  Vector.empty,
                  Vector.empty,
                  None,
                  Some(true),
                  None
              ) -> FileValue(location = Some(f.toString)),
              CommandInputParameter(
                  Some(Identifier(namespace = None, frag = Some("d"))),
                  None,
                  None,
                  CwlDirectory,
                  None,
                  None,
                  Vector.empty,
                  Vector.empty,
                  None,
                  Some(true),
                  Some(LoadListing.Shallow)
              ) -> DirectoryValue(location = Some(d.toString))
          )
      )
      )
      evaluator("$(inputs.f.path)", CwlString, ctx2)._2 shouldBe StringValue(f.toString)
      evaluator("$(inputs.f.basename)", CwlString, ctx2)._2 shouldBe StringValue("test.txt")
      evaluator("$(inputs.f.dirname)", CwlString, ctx2)._2 shouldBe StringValue(tmpdir.toString)
      evaluator("$(inputs.f.nameroot)", CwlString, ctx2)._2 shouldBe StringValue("test")
      evaluator("$(inputs.f.nameext)", CwlString, ctx2)._2 shouldBe StringValue(".txt")
      evaluator("$(inputs.f.size)", CwlLong, ctx2)._2 shouldBe LongValue(4)
      evaluator("$(inputs.f.contents)", CwlString, ctx2)._2 shouldBe StringValue("test")

      evaluator("$(inputs.d.path)", CwlString, ctx2)._2 shouldBe StringValue(d.toString)
      evaluator("$(inputs.d.basename)", CwlString, ctx2)._2 shouldBe StringValue("dir")
      evaluator("$(inputs.d.listing[0].path)", CwlString, ctx2)._2 shouldBe StringValue(f2.toString)
      evaluator("$(inputs.d.listing[0].contents)", CwlString, ctx2)._2 shouldBe StringValue("test2")
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
        val (cwlType, _) = CwlType.translate(getString(testCase("type")))
        val outputBinding =
          testCase("outputBinding").asInstanceOf[java.util.Map[String, Any]].asScala
        val expr = getString(outputBinding("outputEval"))
        evaluator(expr, cwlType, ctx)
      }
    }
  }
}
