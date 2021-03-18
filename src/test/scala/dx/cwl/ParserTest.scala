package dx.cwl

import java.nio.file.{Path, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{File, FilenameFilter}

class ParserTest extends AnyWordSpec with Matchers {
  def getPath(path: String): Path = {
    Paths.get(getClass.getResource(path).getPath)
  }

  "parser" should {
    "not parse an invalid document" in {
      Parser.default.detectVersionAndClass(getPath("/tools/v1.2/invalid1.cwl")) shouldBe None
      Parser.default.detectVersionAndClass(getPath("/tools/v1.2/invalid2.cwl")) shouldBe None
    }

    "parse requirements" in {
      val doc = Parser.default.parseFile(getPath("/tools/v1.2/writable-dir.cwl"))
      doc.requirements.size shouldBe 2
      doc.requirements.iterator sameElements Vector(
          InlineJavascriptRequirement(None),
          InitialWorkDirRequirement(
              Vector(
                  DirInitialWorkDirEntry(
                      entry = StringValue("$({class: 'Directory', listing: []})"),
                      entryName = Some(StringValue("emptyWritableDir")),
                      writable = Some(true)
                  )
              )
          )
      )
    }

    "parse schema" in {
      val doc =
        Parser.default.parseFile(getPath("/tools/v1.2/anon_enum_inside_array_inside_schemadef.cwl"))
      val schemaDefRequirement = doc.requirements.collect {
        case req: SchemaDefRequirement => req
      }
      schemaDefRequirement.size shouldBe 1
      val typeDefs = schemaDefRequirement.head.typeDefinitions
      typeDefs.size shouldBe 3
      val records = typeDefs.collect {
        case rec: CwlInputRecord => rec
      }
      records.size shouldBe 1
      records.head.path shouldBe "vcf2maf_params"
    }

    val cwlFilter = new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".cwl")
    }

    val toolsPath = getPath(s"/tools/v1.2")
    val toolsParser = Parser(Some(toolsPath.toUri))
    toolsPath.toFile.listFiles(cwlFilter).toVector.foreach { toolPath =>
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
            case _: CommandLineTool => ()
            case other =>
              throw new AssertionError(s"expected CommandLineTool, not ${other}")
          }
        }
      }
    }

    val workflowsPath = getPath(s"/workflows/v1.2")
    val workflowParser = Parser.create(Some(workflowsPath.toUri))
    workflowsPath.toFile.listFiles(cwlFilter).toVector.foreach { wfPath =>
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
              case _: Workflow => ()
              case other =>
                throw new AssertionError(s"expected Workflow, not ${other}")
            }
          }
        }
      }
    }

  }
}
