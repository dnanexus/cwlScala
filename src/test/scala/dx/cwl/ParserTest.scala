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
      Parser.default.canParse(getPath("/tools/v1.2/invalid1.cwl")) shouldBe false
      Parser.default.canParse(getPath("/tools/v1.2/invalid2.cwl")) shouldBe false
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

    val cwlFilter = new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".cwl")
    }

    val toolsPath = getPath(s"/tools/v1.2")
    val toolsParser = Parser(Some(toolsPath.toUri.toString))
    toolsPath.toFile.listFiles(cwlFilter).toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        if (toolPath.getName.contains("invalid")) {
          assertThrows[Throwable] {
            toolsParser.parseFile(toolPath.toPath)
          }
        } else {
          toolsParser.canParse(toolPath.toPath) shouldBe true
          toolsParser.parseFile(toolPath.toPath) match {
            case _: CommandLineTool => ()
            case other =>
              throw new AssertionError(s"expected CommandLineTool, not ${other}")
          }
        }
      }
    }

    // TODO: these tests do not currently work due to a bug that causes
    //  workflows to be parsed as CommandLineTools
    //  https://github.com/common-workflow-lab/cwljava/issues/37
//    val workflowsPath = getPath(s"/workflows/v1.2")
//    val workflowParser = Parser(Some(workflowsPath.toUri.toString))
//    workflowsPath.toFile.listFiles(cwlFilter).toVector.foreach { wfPath =>
//      s"parse workflow ${wfPath}" in {
//        if (wfPath.getName.contains("invalid")) {
//          assertThrows[Throwable] {
//            toolsParser.parseFile(wfPath.toPath)
//          }
//        } else {
//          workflowParser.canParse(wfPath.toPath) shouldBe true
//          workflowParser.parseFile(wfPath.toPath) match {
//            case _: Workflow => ()
//            case other =>
//              throw new AssertionError(s"expected Workflow, not ${other}")
//          }
//        }
//      }
//    }
  }
}
