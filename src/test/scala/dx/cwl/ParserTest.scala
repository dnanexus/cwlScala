package dx.cwl

import java.nio.file.{Path, Paths}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserTest extends AnyWordSpec with Matchers {
  def getPath(path: String): Path = {
    Paths.get(getClass.getResource(path).getPath)
  }

  "parser" should {
    "not parse an invalid document" in {
      Parser.canParse(getPath("/tools/fail/invalid1.cwl")) shouldBe false
      Parser.canParse(getPath("/tools/fail/invalid2.cwl")) shouldBe false
    }

    "parse requirements" in {
      val doc = CommandLineTool.parse(getPath("/tools/pass/writable-dir.cwl"))
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

    val toolsPath = getPath("/tools/pass")
    toolsPath.toFile.listFiles().toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        if (!toolPath.getName.contains("invalid")) {
          Parser.canParse(toolPath.toPath) shouldBe true
        }
        CommandLineTool.parse(toolPath.toPath)
      }
    }

    val workflowsPath = getPath("/workflows/pass")
    workflowsPath.toFile.listFiles().toVector.foreach { workflowPath =>
      s"parse workflow ${workflowPath}" in {
        if (workflowPath.getName.contains("-wf")) {
          // the workflow folder contains tools that the workflows depend on
          if (!workflowPath.getName.contains("invalid")) {
            Parser.canParse(workflowPath.toPath) shouldBe true
          }
          Workflow.parse(workflowPath.toPath)
        }
      }
    }
  }
}
