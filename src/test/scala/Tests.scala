import java.nio.file.{Path, Paths}

import cwl.Parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFlatSpec with Matchers {
  def getCwlPath(name: String): Path = {
    Paths.get(getClass.getResource(s"/${name}").getPath)
  }

  it should "parse a simple CWL file" in {
    val tool = Parser.parseTool(getCwlPath("simple.cwl"))
    tool.doc shouldBe Some("hello\nworld")
  }
}
