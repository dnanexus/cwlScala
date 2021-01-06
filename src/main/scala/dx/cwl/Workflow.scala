package dx.cwl

import java.nio.file.Path
import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{
  CWLVersion,
  ExpressionToolImpl,
  ExpressionToolOutputParameterImpl,
  InputBindingImpl,
  OperationImpl,
  OperationInputParameterImpl,
  OperationOutputParameterImpl,
  WorkflowImpl,
  WorkflowInputParameterImpl,
  WorkflowOutputParameterImpl,
  WorkflowStepImpl,
  WorkflowStepInputImpl,
  WorkflowStepOutputImpl,
  LinkMergeMethod => LinkMergeMethodEnum,
  PickValueMethod => PickValueMethodEnum,
  Process => ProcessInterface,
  ScatterMethod => ScatterMethodEnum
}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

import scala.jdk.CollectionConverters._

case class WorkflowInputBinding(loadContents: Option[Boolean])

object WorkflowInputBinding {
  def apply(binding: InputBindingImpl): WorkflowInputBinding = {
    WorkflowInputBinding(translateOptional(binding.getLoadContents).map(_.booleanValue()))
  }
}

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

object WorkflowInputParameter {
  def apply(param: WorkflowInputParameterImpl,
            schemaDefs: Map[String, CwlSchema]): WorkflowInputParameter = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    val inputBinding = translateOptional(param.getInputBinding).map {
      case binding: InputBindingImpl => WorkflowInputBinding(binding)
    }
    WorkflowInputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getDefault).map(CwlValue(_, schemaDefs)),
        inputBinding,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        translateOptional(param.getLoadContents).map(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from)
    )
  }

  def applyArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema]): Vector[WorkflowInputParameter] = {
    params.asScala.toVector.map {
      case param: WorkflowInputParameterImpl =>
        WorkflowInputParameter(param, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowInputParameter value ${other}")
    }
  }
}

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

object WorkflowOutputParameter {
  def apply(param: WorkflowOutputParameterImpl,
            schemaDefs: Map[String, CwlSchema]): WorkflowOutputParameter = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    WorkflowOutputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        translateOptionalArray(param.getOutputSource).map(_.toString),
        translateOptional(param.getLinkMerge).map(LinkMergeMethod.from),
        translateOptional(param.getPickValue).map(PickValueMethod.from)
    )
  }

  def applyArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema]): Vector[WorkflowOutputParameter] = {
    params.asScala.toVector.map {
      case param: WorkflowOutputParameterImpl =>
        WorkflowOutputParameter(param, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowOutputParameter value ${other}")
    }
  }
}

object ScatterMethod extends Enumeration {
  type ScatterMethod = Value
  val Dotproduct, NestedCrossproduct, FlatCrossproduct = Value

  def from(scatterMethod: ScatterMethodEnum): ScatterMethod = {
    scatterMethod match {
      case ScatterMethodEnum.DOTPRODUCT          => ScatterMethod.Dotproduct
      case ScatterMethodEnum.NESTED_CROSSPRODUCT => ScatterMethod.NestedCrossproduct
      case ScatterMethodEnum.FLAT_CROSSPRODUCT   => ScatterMethod.FlatCrossproduct
    }
  }
}

case class WorkflowStepInput(id: Option[Identifier],
                             label: Option[String],
                             source: Vector[String],
                             default: Option[CwlValue],
                             valueFrom: Option[CwlValue],
                             linkMerge: Option[LinkMergeMethod.LinkMergeMethod],
                             pickValue: Option[PickValueMethod.PickValueMethod],
                             loadContents: Option[Boolean],
                             loadListing: Option[LoadListing.LoadListing])

object WorkflowStepInput {
  def apply(step: WorkflowStepInputImpl, schemaDefs: Map[String, CwlSchema]): WorkflowStepInput = {
    WorkflowStepInput(
        translateOptional(step.getId).map(Identifier.apply),
        translateOptional(step.getLabel),
        translateOptionalArray(step.getSource).map(_.toString),
        translateOptional(step.getDefault).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(step.getValueFrom).map(CwlValue(_, schemaDefs)),
        translateOptional(step.getLinkMerge).map(LinkMergeMethod.from),
        translateOptional(step.getPickValue).map(PickValueMethod.from),
        translateOptional(step.getLoadContents).map(_.booleanValue()),
        translateOptional(step.getLoadListing).map(LoadListing.from)
    )
  }

  def applyArray(steps: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema]): Vector[WorkflowStepInput] = {
    steps.asScala.toVector.map {
      case param: WorkflowStepInputImpl => WorkflowStepInput(param, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowStepInput value ${other}")
    }
  }
}

case class WorkflowStepOutput(id: Option[Identifier])

object WorkflowStepOutput {
  def apply(step: WorkflowStepOutputImpl): WorkflowStepOutput = {
    WorkflowStepOutput(translateOptional(step.getId).map(Identifier.apply))
  }

  def applyArray(steps: java.util.List[java.lang.Object]): Vector[WorkflowStepOutput] = {
    steps.asScala.toVector.map {
      case param: WorkflowStepOutputImpl => WorkflowStepOutput(param)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowStepOutput value ${other}")
    }
  }
}

sealed trait ProcessLink
case class AnonymousProcessLink(process: Process) extends ProcessLink
case class NameProcessLink(name: String) extends ProcessLink

case class WorkflowStep(id: Option[Identifier],
                        label: Option[String],
                        doc: Option[String],
                        inputs: Vector[WorkflowStepInput],
                        outputs: Vector[WorkflowStepOutput],
                        run: ProcessLink,
                        when: Option[CwlValue],
                        scatter: Vector[String],
                        scatterMethod: Option[ScatterMethod.ScatterMethod],
                        requirements: Vector[Requirement],
                        hints: Vector[Hint])

object WorkflowStep {
  def apply(step: WorkflowStepImpl,
            schemaDefs: Map[String, CwlSchema],
            hintSchemas: Map[String, HintSchema] = Map.empty): WorkflowStep = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(step.getRequirements, schemaDefs)
    val processLink: ProcessLink = step.getRun match {
      case name: String => NameProcessLink(name)
      case process: ProcessInterface =>
        AnonymousProcessLink(Parser.parseDocument(process, None, schemaDefs, hintSchemas))
      case other =>
        throw new RuntimeException(s"unexpected run value ${other} for step ${step}")
    }
    WorkflowStep(
        translateOptional(step.getId).map(Identifier.apply),
        translateOptional(step.getLabel),
        translateDoc(step.getDoc),
        WorkflowStepInput.applyArray(step.getIn, allSchemaDefs),
        WorkflowStepOutput.applyArray(step.getOut),
        processLink,
        translateOptional(step.getWhen).map(CwlValue(_, allSchemaDefs)),
        translateOptionalArray(step.getScatter).map(_.toString),
        translateOptional(step.getScatterMethod).map(ScatterMethod.from),
        requirements,
        Requirement.applyHints(step.getHints, allSchemaDefs, hintSchemas)
    )
  }

  def applyArray(steps: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 hintSchemas: Map[String, HintSchema] = Map.empty): Vector[WorkflowStep] = {
    steps.asScala.toVector.map {
      case step: WorkflowStepImpl => WorkflowStep(step, schemaDefs, hintSchemas)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowStep value ${other}")
    }
  }
}
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
  def apply(workflow: WorkflowImpl,
            source: Option[Path] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty,
            hintSchemas: Map[String, HintSchema] = Map.empty,
            name: Option[String] = None): Workflow = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(workflow.getRequirements, schemaDefs)
    Workflow(
        source.map(_.toString),
        translateOptional(workflow.getCwlVersion),
        Identifier(workflow.getId, name, source),
        translateOptional(workflow.getLabel),
        translateDoc(workflow.getDoc),
        translateOptionalArray(workflow.getIntent).map(translateString),
        WorkflowInputParameter.applyArray(workflow.getInputs, allSchemaDefs),
        WorkflowOutputParameter.applyArray(workflow.getOutputs, allSchemaDefs),
        WorkflowStep.applyArray(workflow.getSteps, allSchemaDefs, hintSchemas),
        requirements,
        Requirement.applyHints(workflow.getHints, allSchemaDefs, hintSchemas)
    )
  }

  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty,
            hintSchemas: Map[String, HintSchema] = Map.empty): Workflow = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull) match {
      case workflow: WorkflowImpl =>
        Workflow(workflow, Some(path), schemaDefs, hintSchemas)
      case other =>
        throw new RuntimeException(s"Expected Workflow, found ${other}")
    }
  }
}

case class ExpressionToolOutputParameter(id: Option[Identifier],
                                         label: Option[String],
                                         doc: Option[String],
                                         types: Vector[CwlType],
                                         secondaryFiles: Vector[SecondaryFile],
                                         format: Option[CwlValue],
                                         streamable: Option[Boolean])

object ExpressionToolOutputParameter {
  def apply(param: ExpressionToolOutputParameterImpl,
            schemaDefs: Map[String, CwlSchema]): ExpressionToolOutputParameter = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    ExpressionToolOutputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue())
    )
  }

  def applyArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema]): Vector[ExpressionToolOutputParameter] = {
    params.asScala.toVector.map {
      case param: ExpressionToolOutputParameterImpl =>
        ExpressionToolOutputParameter(param, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected OperationInputParameter value ${other}")
    }
  }
}

case class ExpressionTool(source: Option[String],
                          cwlVersion: Option[CWLVersion],
                          id: Identifier,
                          label: Option[String],
                          doc: Option[String],
                          intent: Vector[String],
                          inputs: Vector[WorkflowInputParameter],
                          outputs: Vector[ExpressionToolOutputParameter],
                          expression: CwlValue,
                          requirements: Vector[Requirement],
                          hints: Vector[Hint])
    extends Process

object ExpressionTool {
  def apply(expressionTool: ExpressionToolImpl,
            source: Option[Path] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty,
            hintSchemas: Map[String, HintSchema] = Map.empty,
            name: Option[String] = None): ExpressionTool = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(expressionTool.getRequirements, schemaDefs)
    ExpressionTool(
        source.map(_.toString),
        translateOptional(expressionTool.getCwlVersion),
        Identifier(expressionTool.getId, name, source),
        translateOptional(expressionTool.getLabel),
        translateDoc(expressionTool.getDoc),
        translateOptionalArray(expressionTool.getIntent).map(translateString),
        WorkflowInputParameter.applyArray(expressionTool.getInputs, allSchemaDefs),
        ExpressionToolOutputParameter.applyArray(expressionTool.getOutputs, allSchemaDefs),
        CwlValue(expressionTool.getExpression, allSchemaDefs),
        requirements,
        Requirement.applyHints(expressionTool.getHints, allSchemaDefs, hintSchemas)
    )
  }
}

case class OperationInputParameter(id: Option[Identifier],
                                   label: Option[String],
                                   doc: Option[String],
                                   types: Vector[CwlType],
                                   default: Option[CwlValue],
                                   secondaryFiles: Vector[SecondaryFile],
                                   format: Vector[CwlValue],
                                   streamable: Option[Boolean],
                                   loadContents: Option[Boolean],
                                   loadListing: Option[LoadListing.LoadListing])

object OperationInputParameter {
  def apply(param: OperationInputParameterImpl,
            schemaDefs: Map[String, CwlSchema]): OperationInputParameter = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    OperationInputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getDefault).map(CwlValue(_, schemaDefs)),
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        translateOptional(param.getLoadContents).map(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from)
    )
  }

  def applyArray(
      params: java.util.List[java.lang.Object],
      schemaDefs: Map[String, CwlSchema] = Map.empty
  ): Vector[OperationInputParameter] = {
    params.asScala.toVector.map {
      case param: OperationInputParameterImpl => OperationInputParameter(param, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected OperationInputParameter value ${other}")
    }
  }
}

case class OperationOutputParameter(id: Option[Identifier],
                                    label: Option[String],
                                    doc: Option[String],
                                    types: Vector[CwlType],
                                    secondaryFiles: Vector[SecondaryFile],
                                    format: Option[CwlValue],
                                    streamable: Option[Boolean])

object OperationOutputParameter {
  def apply(param: OperationOutputParameterImpl,
            schemaDefs: Map[String, CwlSchema]): OperationOutputParameter = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    OperationOutputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue())
    )
  }

  def applyArray(
      params: java.util.List[java.lang.Object],
      schemaDefs: Map[String, CwlSchema] = Map.empty
  ): Vector[OperationOutputParameter] = {
    params.asScala.toVector.map {
      case param: OperationOutputParameterImpl => OperationOutputParameter(param, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected OperationOutputParameter value ${other}")
    }
  }
}

case class Operation(source: Option[String],
                     cwlVersion: Option[CWLVersion],
                     id: Identifier,
                     label: Option[String],
                     doc: Option[String],
                     intent: Vector[String],
                     inputs: Vector[OperationInputParameter],
                     outputs: Vector[OperationOutputParameter],
                     requirements: Vector[Requirement],
                     hints: Vector[Hint])
    extends Process

object Operation {
  def apply(operation: OperationImpl,
            source: Option[Path] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty,
            hintSchemas: Map[String, HintSchema] = Map.empty,
            name: Option[String] = None): Operation = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(operation.getRequirements, schemaDefs)
    Operation(
        source.map(_.toString),
        translateOptional(operation.getCwlVersion),
        Identifier(operation.getId, name, source),
        translateOptional(operation.getLabel),
        translateDoc(operation.getDoc),
        translateOptionalArray(operation.getIntent).map(translateString),
        OperationInputParameter.applyArray(operation.getInputs, allSchemaDefs),
        OperationOutputParameter.applyArray(operation.getOutputs, allSchemaDefs),
        requirements,
        Requirement.applyHints(operation.getHints, allSchemaDefs, hintSchemas)
    )
  }
}
