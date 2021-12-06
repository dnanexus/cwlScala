package dx.cwl

import dx.util.FileUtils

import java.nio.file.{Path, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spray.json._

import java.io.{File, FilenameFilter}
import java.net.URI

class ParserTest extends AnyWordSpec with Matchers {
  def getPath(path: String): Path = {
    Paths.get(getClass.getResource(path).getPath)
  }

  "parser" should {
    val cwlFilter = new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".cwl.json")
    }

    val toolsPath = getPath("/tools")
    val toolsBaseUri = toolsPath.toUri
    val toolsParser = Parser.create(Some(toolsBaseUri))

    "parse an empty tool" in {
      toolsParser.parseFile(toolsPath.resolve("empty.cwl.json")) match {
        case ParserResult(tool: CommandLineTool, _, _, _) =>
          tool.requirements shouldBe Vector(ShellCommandRequirement)
        case other =>
          throw new Exception(s"expected CommandLineTool, not ${other}")
      }
    }

    "parse inline record schema" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("record-in-format.cwl.json"), isPacked = true)
      val tool = result.mainProcess match {
        case tool: CommandLineTool => tool
        case other                 => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      tool.inputs.size shouldBe 2
      tool.inputs.filter(_.name == "record_input") match {
        case Vector(i) =>
          i.cwlType match {
            case rec: CwlInputRecord =>
              rec.fields.keySet shouldBe Set("f1", "f2")
            case other => throw new Exception(s"expected type CwlInputRecord, not ${other}")
          }
        case other =>
          throw new Exception(s"expected single input with name 'record_input', not ${other}")
      }
    }

    "parse record type with secondaryFiles" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("record-sd-secondaryFiles.cwl.json"),
                              isPacked = true)
      val tool = result.mainProcess match {
        case tool: CommandLineTool => tool
        case other                 => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      tool.inputs.size shouldBe 1
      tool.inputs.head.cwlType match {
        case rec: CwlInputRecord =>
          rec.fields("f1").secondaryFiles.size shouldBe 1
          rec.fields("f1").secondaryFiles.head shouldBe SecondaryFile(StringValue(".s2"),
                                                                      BooleanValue(true))
          rec.fields("f2").secondaryFiles.size shouldBe 1
          rec.fields("f2").secondaryFiles.head shouldBe SecondaryFile(StringValue(".s3"),
                                                                      BooleanValue(true))
        case other => throw new Exception(s"expected input type to be record not ${other}")
      }
    }

    "parse packed tool with imported output parameters" in {
      val result = toolsParser.parseFile(toolsPath.resolve("params2.cwl.json"), isPacked = true)
      val tool = result.mainProcess match {
        case tool: CommandLineTool => tool
        case other                 => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      tool.outputs.size shouldBe 28
      tool.outputs.head.id.flatMap(_.namespace) shouldBe Some(
          s"${Utils.normalizeUri(toolsBaseUri)}#params_inc.yml"
      )
    }

    "parse packed tool with schema" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("formattest2.cwl.json"), isPacked = true)
      result.mainProcess match {
        case _: CommandLineTool => ()
        case other              => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      result.schemas shouldBe Some(
          JsArray(
              JsString("dx://project-Fy9QqgQ0yzZbg9KXKP4Jz6Yq:file-G4gFG7Q0yzZjk476FKy3jjf8")
          )
      )
    }

    def parseToolConformance(parser: Parser, toolFile: File): Unit = {
      parser.detectVersionAndClass(toolFile.toPath) match {
        case Some((_, cls)) if cls == "CommandLineTool" => ()
        case Some(other) =>
          throw new AssertionError(s"expected CommandLineTool v1.2, not ${other}")
        case None =>
          throw new Exception(s"cannot parse ${toolFile}")
      }
      parser.parseFile(toolFile.toPath) match {
        case ParserResult(_: CommandLineTool, _, _, _) => ()
        case other =>
          throw new AssertionError(s"expected CommandLineTool, not ${other}")
      }
    }

    val toolsConformancePath = toolsPath.resolve("conformance")
    val toolsConformanceParser = Parser.create(Some(toolsConformancePath.toUri))
    toolsConformancePath.toFile.listFiles(cwlFilter).toVector.foreach { toolFile =>
      s"parse tool ${toolFile}" in {
        parseToolConformance(toolsConformanceParser, toolFile)
      }
    }

    val toolsConformanceShouldFailPath = toolsPath.resolve("conformance_shouldfail")
    val toolsConformanceShouldFailParser = Parser(Some(toolsConformanceShouldFailPath.toUri))
    toolsConformanceShouldFailPath.toFile.listFiles(cwlFilter).toVector.foreach { toolFile =>
      s"maybe parse tool ${toolFile}" in {
        if (toolFile.getName.contains("invalid")) {
          toolsConformanceShouldFailParser.detectVersionAndClass(toolFile.toPath) shouldBe None
          assertThrows[Throwable] {
            toolsConformanceShouldFailParser.parseFile(toolFile.toPath)
          }
        } else {
          parseToolConformance(toolsConformanceShouldFailParser, toolFile)
        }
      }
    }

    "parse requirements" in {
      val result =
        toolsConformanceParser.parseFile(toolsConformancePath.resolve("writable-dir.cwl"))
      result.mainProcess.requirements.size shouldBe 2
      result.mainProcess.requirements.iterator sameElements Vector(
          InlineJavascriptRequirement(None),
          InitialWorkDirRequirement(
              Vector(
                  DirInitialWorkDirEntry(
                      entry = StringValue("$({class: 'Directory', listing: []})"),
                      entryName = Some(StringValue("emptyWritableDir")),
                      writable = true
                  )
              )
          )
      )
    }

    "parse schema" in {
      val result =
        toolsConformanceParser.parseFile(
            toolsConformancePath.resolve("anon_enum_inside_array_inside_schemadef.cwl")
        )
      val schemaDefRequirement = result.mainProcess.requirements.collect {
        case req: SchemaDefRequirement => req
      }
      schemaDefRequirement.size shouldBe 1
      val typeDefs = schemaDefRequirement.head.typeDefinitions
      typeDefs.size shouldBe 3
      val records = typeDefs.collect {
        case rec: CwlInputRecord => rec
      }
      records.size shouldBe 1
      records.head.frag shouldBe "vcf2maf_params"
    }

    val workflowsPath = getPath(s"/workflows")
    val workflowsParser = Parser.create(Some(workflowsPath.toUri))

    "parse packed workflow" in {
      val wfPathPacked = workflowsPath.resolve("count-lines1-wf-packed.json")
      workflowsParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (wf, _) = workflowsParser.parseFile(wfPathPacked) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "count-lines1-wf-packed"
    }

    "parse packed workflow in a graph" in {
      val wfPathPacked = workflowsPath.resolve("basename-fields-test-packed.json")
      workflowsParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (_, _) = workflowsParser.parseFile(wfPathPacked) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      val stringParser = Parser.create(Some(URI.create("file:/null")))
      val (wf, _) = stringParser.parseString(FileUtils.readFileContent(wfPathPacked),
                                             Some("basename-fields-test")) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "basename-fields-test"
    }

    "parse packed workflow not in a graph" in {
      val wfPathPacked = workflowsPath.resolve("any-type-compat.cwl.json")
      workflowsParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (wf, _) = workflowsParser.parseFile(wfPathPacked, isPacked = true) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "any-type-compat"
      wf.inputs.flatMap(_.id.map(_.frag.get)).toSet shouldBe Set("input1", "input2", "input3")
    }

    "parse packed workflow with JavaScript expressions" in {
      val wfPathPacked = workflowsPath.resolve("timelimit2-wf.cwl.json")
      workflowsParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (wf, _) = workflowsParser.parseFile(wfPathPacked, isPacked = true) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      val out = wf.steps.head.run.outputs.head match {
        case out: CommandOutputParameter => out.outputBinding.get.outputEval.get
        case other                       => throw new Exception(s"expected CommandOutputParameter not ${other}")
      }
      out shouldBe StringValue("$(\"time passed\")")
      val (t, v) = Evaluator(jsEnabled = true).evaluate(out, CwlString, EvaluatorContext.empty)
      t shouldBe CwlString
      v shouldBe StringValue("time passed")
    }

    def parseWorkflowConformance(parser: Parser, wfFile: File): Unit = {
      val isWorkflow = parser.detectVersionAndClass(wfFile.toPath) match {
        case Some((_, cls)) => cls == "Workflow"
        case None           => false
      }
      if (isWorkflow) {
        parser.parseFile(wfFile.toPath) match {
          case ParserResult(_: Workflow, _, _, _) => ()
          case other =>
            throw new AssertionError(s"expected Workflow, not ${other}")
        }
      }
    }

    val workflowsConformancePath = workflowsPath.resolve("conformance")
    val workflowConformanceParser = Parser.create(Some(workflowsConformancePath.toUri))
    workflowsConformancePath.toFile.listFiles(cwlFilter).toVector.foreach { wfFile =>
      s"parse workflow ${wfFile}" in {
        parseWorkflowConformance(workflowConformanceParser, wfFile)
      }
    }

    val workflowsConformanceShouldFailPath = workflowsPath.resolve("conformance_shouldfail")
    val workflowConformanceShouldFailParser =
      Parser.create(Some(workflowsConformanceShouldFailPath.toUri))
    workflowsConformanceShouldFailPath.toFile.listFiles(cwlFilter).toVector.foreach { wfFile =>
      s"maybe parse workflow ${wfFile}" in {
        if (wfFile.getName.contains("invalid")) {
          workflowConformanceShouldFailParser.detectVersionAndClass(wfFile.toPath) shouldBe None
          assertThrows[Throwable] {
            workflowConformanceShouldFailParser.parseFile(wfFile.toPath)
          }
        } else {
          parseWorkflowConformance(workflowConformanceShouldFailParser, wfFile)
        }
      }
    }
  }
}
