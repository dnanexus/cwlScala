package dx.cwl

import java.nio.file.Path

import org.w3id.cwl.cwl1_2.WorkflowImpl
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

case class Workflow(source: Option[String]) extends Process

object Workflow {
  def apply(workflow: WorkflowImpl, source: Option[String] = None): Workflow = {
    throw new NotImplementedError(s"parsing Workflows is not yet supported")
  }

  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None): Workflow = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull) match {
      case workflow: WorkflowImpl => Workflow(workflow, Some(path.toString))
      case other =>
        throw new RuntimeException(s"Expected Workflow, found ${other}")
    }
  }
}
