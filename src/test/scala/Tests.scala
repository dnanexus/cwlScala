import java.nio.file.Paths

import cwl.CommandLineTool
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Tests extends AnyWordSpec with Matchers {
  "parser" should {
    val toolsPath = Paths.get(getClass.getResource(s"/tools/pass").getPath)
    toolsPath.toFile.listFiles().toVector.foreach { toolPath =>
      s"parse tool ${toolPath}" in {
        CommandLineTool.parse(toolPath.toPath)
      }
    }
  }
}
