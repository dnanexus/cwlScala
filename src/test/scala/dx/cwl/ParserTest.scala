package dx.cwl

import java.nio.file.{Path, Paths}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserTest extends AnyWordSpec with Matchers {
  def getPath(path: String): Path = {
    Paths.get(getClass.getResource(path).getPath)
  }

  private val parser = Parser.default

  "parser" should {
    "not parse an invalid document" in {
      parser.canParse(getPath("/tools/fail/invalid1.cwl")) shouldBe false
      parser.canParse(getPath("/tools/fail/invalid2.cwl")) shouldBe false
    }

    "parse requirements" in {
      val doc = parser.parse(getPath("/tools/pass/writable-dir.cwl"))
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

    val toolsPath = getPath(s"/tools/pass")
    toolsPath.toFile.listFiles().toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        if (!toolPath.getName.contains("invalid")) {
          parser.canParse(toolPath.toPath) shouldBe true
        }
        parser.parse(toolPath.toPath) match {
          case _: CommandLineTool => ()
          case other              => throw new AssertionError(s"expected CommandLineTool, not ${other}")
        }
      }
    }
  }
}
