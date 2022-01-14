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

    val toolsPath = getPath("/CommandLineTools")
    val toolsBaseUri = toolsPath.toUri
    val toolsParser = Parser.create(Some(toolsBaseUri))

    "parse an empty tool" in {
      toolsParser.parseFile(toolsPath.resolve("empty.cwl.json")) match {
        case ParserResult(Some(tool: CommandLineTool), _, _, _) =>
          tool.requirements shouldBe Vector(ShellCommandRequirement)
        case other =>
          throw new Exception(s"expected CommandLineTool, not ${other}")
      }
    }

    "parse inline record schema" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("record-in-format.cwl.json"), isGraph = true)
      val tool = result.mainProcess match {
        case Some(tool: CommandLineTool) => tool
        case other                       => throw new Exception(s"expected CommandLineTool, not ${other}")
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
                              isGraph = true)
      val tool = result.mainProcess match {
        case Some(tool: CommandLineTool) => tool
        case other                       => throw new Exception(s"expected CommandLineTool, not ${other}")
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
      val result = toolsParser.parseFile(toolsPath.resolve("params2.cwl.json"), isGraph = true)
      val tool = result.mainProcess match {
        case Some(tool: CommandLineTool) => tool
        case other                       => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      tool.outputs.size shouldBe 28
      tool.outputs.head.id.flatMap(_.namespace) shouldBe Some(
          s"${Utils.normalizeUri(toolsBaseUri)}#params_inc.yml"
      )
    }

    "parse packed tool with schema" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("formattest2.cwl.json"), isGraph = true)
      result.mainProcess match {
        case Some(_: CommandLineTool) => ()
        case other                    => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      result.schemas shouldBe Some(
          JsArray(
              JsString("dx://project-Fy9QqgQ0yzZbg9KXKP4Jz6Yq:file-G4gFG7Q0yzZjk476FKy3jjf8")
          )
      )
    }

    def parseToolConformance(parser: Parser, toolFile: File): Unit = {
      parser.detectVersionAndClassFromFile(toolFile.toPath) match {
        case (_, Some(cls)) if cls == "CommandLineTool" => ()
        case (_, Some(other)) =>
          throw new AssertionError(s"expected CommandLineTool, not ${other}")
        case (_, None) =>
          throw new Exception(s"cannot parse ${toolFile}")
      }
      parser.parseFile(toolFile.toPath) match {
        case ParserResult(Some(_: CommandLineTool), _, _, _) => ()
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
          toolsConformanceShouldFailParser
            .detectVersionAndClassFromFile(toolFile.toPath) shouldBe None
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
      result.mainProcess.get.requirements.size shouldBe 2
      result.mainProcess.get.requirements.iterator sameElements Vector(
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
      val schemaDefRequirement = result.mainProcess.get.requirements.collect {
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

    "parse anonymous enum type in packed tool" in {
      val tool = toolsConformanceParser.parseFile(
          toolsConformancePath.resolve("anon_enum_inside_array_inside_schemadef.cwl.json")
      ) match {
        case ParserResult(Some(tool: CommandLineTool), _, _, _) => tool
        case other                                              => throw new Exception(s"expected CommandLineTool not ${other}")
      }
      tool.inputs.size shouldBe 1
      val recType = tool.inputs.head.cwlType match {
        case rec: CwlInputRecord => rec
        case other               => throw new Exception(s"expected CwlInputRecord not ${other}")
      }
      recType.fields.size shouldBe 2
      recType.fields("species").cwlType match {
        case CwlOptional(e: CwlEnum) =>
          e.symbols shouldBe Vector("homo_sapiens", "mus_musculus")
        case other => throw new Exception(s"expected CwlOptional(CwlEnum) not ${other}")
      }
      recType.fields("ncbi_build").cwlType match {
        case CwlOptional(e: CwlEnum) =>
          e.symbols shouldBe Vector("GRCh37", "GRCh38", "GRCm38")
        case other => throw new Exception(s"expected CwlOptional(CwlEnum) not ${other}")
      }
    }

    val expressionToolsConformancePath = getPath("/ExpressionTools/conformance")
    val expressionToolsConformanceParser = Parser.create(Some(expressionToolsConformancePath.toUri))
    expressionToolsConformancePath.toFile.listFiles(cwlFilter).toVector.foreach { toolFile =>
      s"parse tool ${toolFile}" in {
        expressionToolsConformanceParser.detectVersionAndClassFromFile(toolFile.toPath) match {
          case (_, Some(cls)) if cls == "ExpressionTool" => ()
          case (_, Some(other)) =>
            throw new AssertionError(s"expected ExpressionTool, not ${other}")
          case (_, None) =>
            throw new Exception(s"cannot parse ${toolFile}")
        }
        expressionToolsConformanceParser.parseFile(toolFile.toPath) match {
          case ParserResult(Some(_: ExpressionTool), _, _, _) => ()
          case other =>
            throw new AssertionError(s"expected ExpressionTool, not ${other}")
        }
      }
    }

    val workflowsPath = getPath(s"/Workflows")
    val workflowsParser = Parser.create(Some(workflowsPath.toUri))

    "parse packed workflow" in {
      val wfPathPacked = workflowsPath.resolve("count-lines1-wf-packed.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      val (wf, _) = workflowsParser.parseFile(wfPathPacked) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "count-lines1-wf-packed"
    }

    "parse packed workflow in a graph" in {
      val wfPathPacked = workflowsPath.resolve("basename-fields-test-packed.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      val (_, _) = workflowsParser.parseFile(wfPathPacked) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
      }
      val stringParser = Parser.create(Some(URI.create("file:/null")))
      val (wf, _) = stringParser.parseString(FileUtils.readFileContent(wfPathPacked),
                                             Some("basename-fields-test")) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "basename-fields-test"
    }

    "parse packed workflow not in a graph" in {
      val wfPathPacked = workflowsPath.resolve("any-type-compat.cwl.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      val (wf, _) = workflowsParser.parseFile(wfPathPacked, isGraph = true) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "any-type-compat"
      wf.inputs.flatMap(_.id.map(_.frag)).toSet shouldBe Set("input1", "input2", "input3")
    }

    "parse packed workflow with JavaScript expressions" in {
      val wfPathPacked = workflowsPath.resolve("timelimit2-wf.cwl.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      val (wf, _) = workflowsParser.parseFile(wfPathPacked, isGraph = true) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
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

    "parse packed workflow with auto-generated process ID" in {
      val wfPathPacked = workflowsPath.resolve("conformance").resolve("count-lines19-wf.cwl.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      val (wf, _) = workflowsParser.parseFile(wfPathPacked, simplifyProcessAutoIds = true) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.id.map(_.frag) shouldBe Some("count-lines19-wf")
      val proc = wf.steps.head.run match {
        case tool: CommandLineTool => tool
        case _                     => throw new Exception("expected CommandLineTool")
      }
      proc.id.map(_.frag) shouldBe Some("wc3-tool")
    }

    "parse packed workflow with auto-generated anonymous process ID" in {
      val wfPathPacked = workflowsPath.resolve("conformance").resolve("timelimit2-wf.cwl.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      val (wf, _) = workflowsParser.parseFile(wfPathPacked, simplifyProcessAutoIds = true) match {
        case ParserResult(Some(wf: Workflow), doc, _, _) => (wf, doc)
        case other                                       => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.id.map(_.frag) shouldBe Some("timelimit2-wf")
      wf.steps.zipWithIndex.foreach {
        case (step, idx) =>
          step.run match {
            case tool: CommandLineTool => tool.id.map(_.frag) shouldBe Some(s"step${idx + 1}_run")
            case _                     => throw new Exception("expected CommandLineTool")
          }
      }
    }

    "parse packed workflow with two imports of same process" in {
      val wfPathPacked =
        workflowsPath.resolve("conformance").resolve("basename-fields-test.cwl.json")
      workflowsParser.detectVersionAndClassFromFile(wfPathPacked) shouldBe ("v1.2", Some(
          "Workflow"
      ))
      // this will throw an exception unless the two processes with the same name are identical
      workflowsParser.parseFile(wfPathPacked, simplifyProcessAutoIds = true)
    }

    def parseWorkflowConformance(parser: Parser,
                                 wfFile: File,
                                 mainIds: Map[Path, Identifier] = Map.empty): Unit = {
      val wfPath = wfFile.toPath
      val isWorkflow = parser.detectVersionAndClassFromFile(wfPath) match {
        case (_, Some(cls)) => cls == "Workflow"
        case _              => false
      }
      if (isWorkflow) {
        val mainId = mainIds.getOrElse(wfPath, Identifier.MainId)
        parser.parseFile(wfFile.toPath, mainId = mainId) match {
          case ParserResult(Some(_: Workflow), _, _, _) => ()
          case other =>
            throw new AssertionError(s"expected Workflow, not ${other}")
        }
      }
    }

    val workflowsConformancePath = workflowsPath.resolve("conformance")
    val workflowConformanceParser = Parser.create(Some(workflowsConformancePath.toUri))
    // Some workflow conformance tests are in $graph format but do not have a #main process; instead, the main process
    // id is specified as part of the tool name in conformance_tests.yaml.
    val workflowMainProcesses = Map(
        "conflict-wf.cwl.json" -> "collision",
        "js-expr-req-wf.cwl.json" -> "wf"
//        "revsort-packed.cwl.json" -> "main",
//        "scatter-valuefrom-wf3.cwl.json" -> "main",
//        "scatter-valuefrom-wf4.cwl.json" -> "main",
//        "scatter-wf3.cwl.json" -> "main",
//        "scatter-wf4.cwl.json" -> "main",
//        "search.cwl.json" -> "main"
    ).map {
      case (filename, frag) =>
        workflowsConformancePath.resolve(filename) -> Identifier(None, frag)
    }
    workflowsConformancePath.toFile.listFiles(cwlFilter).toVector.foreach { wfFile =>
      s"parse workflow ${wfFile}" in {
        parseWorkflowConformance(workflowConformanceParser, wfFile, workflowMainProcesses)
      }
    }

    val workflowsConformanceShouldFailPath = workflowsPath.resolve("conformance_shouldfail")
    val workflowConformanceShouldFailParser =
      Parser.create(Some(workflowsConformanceShouldFailPath.toUri))
    workflowsConformanceShouldFailPath.toFile.listFiles(cwlFilter).toVector.foreach { wfFile =>
      s"maybe parse workflow ${wfFile}" in {
        if (wfFile.getName.contains("invalid")) {
          workflowConformanceShouldFailParser
            .detectVersionAndClassFromFile(wfFile.toPath) shouldBe None
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
