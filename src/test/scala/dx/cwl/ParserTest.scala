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
      Parser.default.canParse(getPath("/tools/pass/invalid1.cwl")) shouldBe false
      Parser.default.canParse(getPath("/tools/pass/invalid2.cwl")) shouldBe false
    }

    "parse requirements" in {
      val doc = Parser.default.parseFile(getPath("/tools/pass/writable-dir.cwl"))
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

    val passToolsPath = getPath(s"/tools/pass")
    val passParser = Parser(Some(passToolsPath.toUri.toString))
    passToolsPath.toFile.listFiles(cwlFilter).toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        if (toolPath.getName.contains("invalid")) {
          assertThrows[Throwable] {
            passParser.parseFile(toolPath.toPath)
          }
        } else {
          passParser.canParse(toolPath.toPath) shouldBe true
          passParser.parseFile(toolPath.toPath) match {
            case _: CommandLineTool => ()
            case other =>
              throw new AssertionError(s"expected CommandLineTool, not ${other}")
          }
        }
      }
    }

//    val failToolsPath = getPath(s"/tools/fail")
//    val failParser = Parser(Some(failToolsPath.toUri.toString))
//    failToolsPath.toFile.listFiles(cwlFilter).toVector.foreach { toolPath =>
//      s"parse tool ${toolPath}" in {
//        if (toolPath.getName.contains("invalid")) {
//          assertThrows[Throwable] {
//            failParser.parseFile(toolPath.toPath)
//          }
//        } else {
//          failParser.canParse(toolPath.toPath) shouldBe true
//          failParser.parseFile(toolPath.toPath) match {
//            case _: CommandLineTool => ()
//            case other =>
//              throw new AssertionError(s"expected CommandLineTool, not ${other}")
//          }
//        }
//      }
//    }
  }
}
