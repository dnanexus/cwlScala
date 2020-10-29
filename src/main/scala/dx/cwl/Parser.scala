package dx.cwl

import java.nio.file.Path

import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

trait DocumentElement {
  val source: Option[String]
}

object Parser {
  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None): DocumentElement = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull) match {
      case tool: CommandLineToolImpl => CommandLineTool(tool)
      case workflow: WorkflowImpl    => Workflow(workflow)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other}")
    }
  }
}
