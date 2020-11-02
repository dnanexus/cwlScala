package dx.cwl

import java.nio.file.Path

import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

/**
  * Marker trait for top-level elements (CommandLineTool, Workflow, ExpressionTool, etc)
  */
trait Process {
  val source: Option[String]
}

object Parser {

  /**
    * Parses a CWL document.
    * @note currently, only CommandLineTool documents are supported.
    * @param path path to the document
    * @param baseUri base URI to use when importing documents
    * @param loadingOptions document loading options
    * @return a [[Process]]
    */
  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None): Process = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull) match {
      case tool: CommandLineToolImpl => CommandLineTool(tool)
      case workflow: WorkflowImpl    => Workflow(workflow)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other}")
    }
  }
}
