package cwl

import java.nio.file.Path

import org.w3id.cwl.cwl1_2_0_dev3.{CommandLineTool, Workflow}
import org.w3id.cwl.cwl1_2_0_dev3.utils.{LoadingOptions, RootLoader}

object Parser {
  /**
   * Parse a CWL document.
   *
   * @param path path to the CWL document.
   * @param baseUri base path to make paths absolute - defaults to cwd
   * @param loadingOptions options to use for document loading
   * @return one of the top.level CWL object types (CommandLineTool, ExpressionTool, Workflow, Operation, or an
   *         array of these)
   */
  def parse(path: Path, baseUri: Option[String] = None, loadingOptions: Option[LoadingOptions] = None): Any = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull)
  }

  def parseTool(path: Path, baseUri: Option[String] = None, loadingOptions: Option[LoadingOptions] = None): CommandLineTool = {
    parse(path, baseUri, loadingOptions) match {
      case tool: CommandLineTool => tool
      case other => throw new RuntimeException(s"Expected CommandLineTool, found ${other}")
    }
  }

  def parseWorkflow(path: Path, baseUri: Option[String] = None, loadingOptions: Option[LoadingOptions] = None): Workflow = {
    parse(path, baseUri, loadingOptions) match {
      case workflow: Workflow => workflow
      case other => throw new RuntimeException(s"Expected Workflow, found ${other}")
    }
  }
}
