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
      override def accept(dir: File, name: String): Boolean = name.endsWith(".cwl")
    }

    val toolsPath = getPath("/tools")
    val tools12Path = toolsPath.resolve("v1.2")
    val baseToolUri = tools12Path.toUri
    val toolsParser = Parser(Some(baseToolUri))
    tools12Path.toFile.listFiles(cwlFilter).toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        if (toolPath.getName.contains("invalid")) {
          assertThrows[Throwable] {
            toolsParser.parseFile(toolPath.toPath)
          }
        } else {
          toolsParser.detectVersionAndClass(toolPath.toPath) match {
            case Some((_, cls)) if cls == "CommandLineTool" => ()
            case Some(other) =>
              throw new AssertionError(s"expected CommandLineTool v1.2, not ${other}")
            case None =>
              throw new Exception(s"cannot parse ${toolPath}")
          }
          toolsParser.parseFile(toolPath.toPath) match {
            case ParserResult(_: CommandLineTool, _, _, _) => ()
            case other =>
              throw new AssertionError(s"expected CommandLineTool, not ${other}")
          }
        }
      }
    }

    "parse an empty tool" in {
      toolsParser.parseFile(toolsPath.resolve("empty.cwl.json")) match {
        case ParserResult(tool: CommandLineTool, _, _, _) =>
          tool.requirements shouldBe Vector(ShellCommandRequirement)
        case other =>
          throw new Exception(s"expected CommandLineTool, not ${other}")
      }
    }

    "not parse an invalid document" in {
      toolsParser.detectVersionAndClass(tools12Path.resolve("invalid1.cwl")) shouldBe None
      toolsParser.detectVersionAndClass(tools12Path.resolve("invalid2.cwl")) shouldBe None
    }

    "parse requirements" in {
      val result = toolsParser.parseFile(tools12Path.resolve("writable-dir.cwl"))
      result.process.requirements.size shouldBe 2
      result.process.requirements.iterator sameElements Vector(
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
        toolsParser.parseFile(tools12Path.resolve("anon_enum_inside_array_inside_schemadef.cwl"))
      val schemaDefRequirement = result.process.requirements.collect {
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

    "parse inline record schema" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("record-in-format.cwl.json"), isPacked = true)
      val tool = result.process match {
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
      val tool = result.process match {
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
      val tool = result.process match {
        case tool: CommandLineTool => tool
        case other                 => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      tool.outputs.size shouldBe 28
      tool.outputs.head.id.flatMap(_.namespace) shouldBe Some(
          s"${Utils.normalizeUri(baseToolUri)}#params_inc.yml"
      )
    }

    "parse packed tool with schema" in {
      val result =
        toolsParser.parseFile(toolsPath.resolve("formattest2.cwl.json"), isPacked = true)
      result.process match {
        case _: CommandLineTool => ()
        case other              => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      result.schemas shouldBe Some(
          JsArray(
              JsString("dx://project-Fy9QqgQ0yzZbg9KXKP4Jz6Yq:file-G4gFG7Q0yzZjk476FKy3jjf8")
          )
      )
    }

    val workflowsPath = getPath(s"/workflows")
    val workflows12Path = workflowsPath.resolve(s"v1.2")
    val workflowParser = Parser.create(Some(workflows12Path.toUri))
    workflows12Path.toFile.listFiles(cwlFilter).toVector.foreach { wfPath =>
      s"parse workflow ${wfPath}" in {
        if (wfPath.getName.contains("invalid")) {
          assertThrows[Throwable] {
            workflowParser.parseFile(wfPath.toPath)
          }
        } else {
          val isWorkflow = workflowParser.detectVersionAndClass(wfPath.toPath) match {
            case Some((_, cls)) => cls == "Workflow"
            case None           => false
          }
          if (isWorkflow) {
            workflowParser.parseFile(wfPath.toPath) match {
              case ParserResult(_: Workflow, _, _, _) => ()
              case other =>
                throw new AssertionError(s"expected Workflow, not ${other}")
            }
          }
        }
      }
    }

    "parse packed workflow" in {
      val wfPathPacked = workflowsPath.resolve("count-lines1-wf-packed.json")
      workflowParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (wf, _) = workflowParser.parseFile(wfPathPacked) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "count-lines1-wf-packed"
    }

    "parse packed workflow in a graph" in {
      val wfPathPacked = workflowsPath.resolve("basename-fields-test-packed.json")
      workflowParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (_, _) = workflowParser.parseFile(wfPathPacked) match {
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
      workflowParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (wf, _) = workflowParser.parseFile(wfPathPacked, isPacked = true) match {
        case ParserResult(wf: Workflow, doc, _, _) => (wf, doc)
        case other                                 => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "any-type-compat"
      wf.inputs.flatMap(_.id.map(_.frag.get)).toSet shouldBe Set("input1", "input2", "input3")
    }
  }
}
