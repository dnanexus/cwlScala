import java.nio.file.Paths

import dx.cwl.{CommandLineTool, Parser}

object ParserExample {
  val tool: CommandLineTool =
    Parser.parse(Paths.get("../../test/resources/tools/pass/action.cwl")) match {
      case tool: CommandLineTool => tool
      case _                     => throw new Exception("not a CommandLineTool")
    }
  tool.inputs.map { param =>
    println(s"${param.id.get}: ${param.types}")
  }
}
