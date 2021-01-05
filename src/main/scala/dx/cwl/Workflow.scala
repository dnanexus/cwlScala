package dx.cwl

import java.nio.file.Path
import org.w3id.cwl.cwl1_2.{
  CWLVersion,
  WorkflowImpl,
  LinkMergeMethod => LinkMergeMethodEnum,
  PickValueMethod => PickValueMethodEnum
}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

case class WorkflowInputBinding(loadContents: Option[Boolean])

case class WorkflowInputParameter(id: Option[Identifier],
                                  label: Option[String],
                                  doc: Option[String],
                                  types: Vector[CwlType],
                                  default: Option[CwlValue],
                                  inputBinding: Option[WorkflowInputBinding],
                                  secondaryFiles: Vector[SecondaryFile],
                                  format: Vector[CwlValue],
                                  streamable: Option[Boolean],
                                  loadContents: Option[Boolean],
                                  loadListing: Option[LoadListing.LoadListing])

object LinkMergeMethod extends Enumeration {
  type LinkMergeMethod = Value
  val MergeNested, MergeFlattened = Value

  def from(linkMergeMethod: LinkMergeMethodEnum): LinkMergeMethod.LinkMergeMethod = {
    linkMergeMethod match {
      case LinkMergeMethodEnum.MERGE_NESTED    => MergeNested
      case LinkMergeMethodEnum.MERGE_FLATTENED => MergeFlattened
    }
  }
}

object PickValueMethod extends Enumeration {
  type PickValueMethod = Value
  val AllNonNull, FirstNonNull, TheOnlyNonNull = Value

  def from(linkMergeMethod: PickValueMethodEnum): PickValueMethod.PickValueMethod = {
    linkMergeMethod match {
      case PickValueMethodEnum.ALL_NON_NULL      => AllNonNull
      case PickValueMethodEnum.FIRST_NON_NULL    => FirstNonNull
      case PickValueMethodEnum.THE_ONLY_NON_NULL => TheOnlyNonNull
    }
  }
}

case class WorkflowOutputParameter(id: Option[Identifier],
                                   label: Option[String],
                                   doc: Option[String],
                                   types: Vector[CwlType],
                                   secondaryFiles: Vector[SecondaryFile],
                                   format: Option[CwlValue],
                                   streamable: Option[Boolean],
                                   outputSource: Vector[String],
                                   linkMerge: Option[LinkMergeMethod.LinkMergeMethod],
                                   pickValue: Option[PickValueMethod.PickValueMethod])
case class WorkflowStep()

case class Workflow(source: Option[String],
                    cwlVersion: Option[CWLVersion],
                    id: Identifier,
                    label: Option[String],
                    doc: Option[String],
                    intent: Vector[String],
                    inputs: Vector[WorkflowInputParameter],
                    outputs: Vector[WorkflowOutputParameter],
                    steps: Vector[WorkflowStep],
                    requirements: Vector[Requirement],
                    hints: Vector[Hint])
    extends Process

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
