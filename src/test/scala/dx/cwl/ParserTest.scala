package dx.cwl

import java.nio.file.{Path, Paths}

import org.scalatest.Ignore
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

    val toolsPath = getPath(s"/tools/pass")
    toolsPath.toFile.listFiles().toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        if (!toolPath.getName.contains("invalid")) {
          Parser.canParse(toolPath.toPath) shouldBe true
        }
        CommandLineTool.parse(toolPath.toPath)
      }
    }

    // this test is broken due to a bug in the underlying java parser
//    "parse requirements" in {
//      val doc = CommandLineTool.parse(getPath("/tools/pass/writable-dir.cwl"))
//      doc.requirements.size shouldBe 2
//      val workDir: Map[String, CwlValue] = Map(
//          "entryname" -> StringValue("emptyWritableDir"),
//          "writable" -> BooleanValue(true),
//          "entry" -> StringValue("$({class: 'Directory', listing: []})")
//      )
//      doc.requirements shouldBe Vector(
//          InlineJavascriptRequirement(None),
//          InitialWorkDirRequirement(Vector(ObjectValue(workDir)))
//      )
//    }
  }
}
