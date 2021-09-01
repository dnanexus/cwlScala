package dx.cwl

import dx.util.FileUtils

import java.nio.file.{Path, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

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
            case (_: CommandLineTool, _) => ()
            case other =>
              throw new AssertionError(s"expected CommandLineTool, not ${other}")
          }
        }
      }
    }

    "not parse an invalid document" in {
      toolsParser.detectVersionAndClass(tools12Path.resolve("invalid1.cwl")) shouldBe None
      toolsParser.detectVersionAndClass(tools12Path.resolve("invalid2.cwl")) shouldBe None
    }

    "parse requirements" in {
      val (proc, _) = toolsParser.parseFile(tools12Path.resolve("writable-dir.cwl"))
      proc.requirements.size shouldBe 2
      proc.requirements.iterator sameElements Vector(
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
      val (proc, _) =
        toolsParser.parseFile(tools12Path.resolve("anon_enum_inside_array_inside_schemadef.cwl"))
      val schemaDefRequirement = proc.requirements.collect {
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
      val (proc, _) =
        toolsParser.parseFile(toolsPath.resolve("record-in-format.cwl.json"), isPacked = true)
      val tool = proc match {
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

    "parse packed tool with imported output parameters" in {
      val (proc, _) = toolsParser.parseFile(toolsPath.resolve("params2.cwl.json"), isPacked = true)
      val tool = proc match {
        case tool: CommandLineTool => tool
        case other                 => throw new Exception(s"expected CommandLineTool, not ${other}")
      }
      tool.outputs.size shouldBe 28
      tool.outputs.head.id.flatMap(_.namespace) shouldBe Some(
          s"${Utils.normalizeUri(baseToolUri)}#params_inc.yml"
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
              case (_: Workflow, _) => ()
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
        case (wf: Workflow, doc) => (wf, doc)
        case other               => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "count-lines1-wf-packed"
    }

    "parse packed workflow in a graph" in {
      val wfPathPacked = workflowsPath.resolve("basename-fields-test-packed.json")
      workflowParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (_, _) = workflowParser.parseFile(wfPathPacked) match {
        case (wf: Workflow, doc) => (wf, doc)
        case other               => throw new Exception(s"expected Workflow, not ${other}")
      }
      val stringParser = Parser.create(Some(URI.create("file:/null")))
      val (wf, _) = stringParser.parseString(FileUtils.readFileContent(wfPathPacked),
                                             Some("basename-fields-test")) match {
        case (wf: Workflow, doc) => (wf, doc)
        case other               => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "basename-fields-test"
    }

    "parse packed workflow not in a graph" in {
      val wfPathPacked = workflowsPath.resolve("any-type-compat.cwl.json")
      workflowParser.detectVersionAndClass(wfPathPacked) shouldBe Some("v1.2", "Workflow")
      val (wf, _) = workflowParser.parseFile(wfPathPacked, isPacked = true) match {
        case (wf: Workflow, doc) => (wf, doc)
        case other               => throw new Exception(s"expected Workflow, not ${other}")
      }
      wf.name shouldBe "any-type-compat"
      wf.inputs.flatMap(_.id.map(_.frag.get)).toSet shouldBe Set("input1", "input2", "input3")
    }
  }
}
