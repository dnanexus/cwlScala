package dx.cwl

import java.nio.file.Path
import dx.cwl.Document.{Document, DocumentAdder}
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
                                  cwlType: CwlType,
                                  default: Option[CwlValue],
                                  inputBinding: Option[WorkflowInputBinding],
                                  secondaryFiles: Vector[SecondaryFile],
                                  format: Vector[CwlValue],
                                  streamable: Option[Boolean],
                                  loadContents: Option[Boolean],
                                  loadListing: Option[LoadListing.LoadListing])
    extends InputParameter

object WorkflowInputParameter {
  def apply(param: WorkflowInputParameterImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowInputParameter = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    val inputBinding = translateOptional(param.getInputBinding).map {
      case binding: InputBindingImpl => WorkflowInputBinding(binding)
    }
    WorkflowInputParameter(
        translateOptional(param.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
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
                 schemaDefs: Map[String, CwlSchema],
                 stripPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowInputParameter] = {
    params.asScala.toVector.map {
      case param: WorkflowInputParameterImpl =>
        WorkflowInputParameter(param, schemaDefs, stripPrefix, defaultNamespace)
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
                                   cwlType: CwlType,
                                   secondaryFiles: Vector[SecondaryFile],
                                   format: Option[CwlValue],
                                   streamable: Option[Boolean],
                                   outputSource: Vector[Identifier],
                                   linkMerge: Option[LinkMergeMethod.LinkMergeMethod],
                                   pickValue: Option[PickValueMethod.PickValueMethod])
    extends OutputParameter

object WorkflowOutputParameter {
  def apply(param: WorkflowOutputParameterImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowOutputParameter = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    val id =
      translateOptional(param.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace))
    val sources =
      translateOptionalArray(param.getOutputSource)
        .map(source => Identifier.parse(source.toString, stripFragPrefix, defaultNamespace))
        .map { src =>
          // TODO: this is a work-around for a parser bug - remove when fixed
          if (id.exists(_.frag.isDefined) && src.frag.isDefined) {
            val prefix = s"${id.flatMap(_.frag).get}/"
            if (src.frag.get.startsWith(prefix)) {
              src.copy(frag = src.frag.map(_.drop(prefix.length)))
            } else {
              src
            }
          } else {
            src
          }
        }
    WorkflowOutputParameter(
        id,
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        sources,
        translateOptional(param.getLinkMerge).map(LinkMergeMethod.from),
        translateOptional(param.getPickValue).map(PickValueMethod.from)
    )
  }

  def applyArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowOutputParameter] = {
    params.asScala.toVector.map {
      case param: WorkflowOutputParameterImpl =>
        WorkflowOutputParameter(param, schemaDefs, stripFragPrefix, defaultNamespace)
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
                             source: Vector[Identifier],
                             default: Option[CwlValue],
                             valueFrom: Option[CwlValue],
                             linkMerge: Option[LinkMergeMethod.LinkMergeMethod],
                             pickValue: Option[PickValueMethod.PickValueMethod],
                             loadContents: Option[Boolean],
                             loadListing: Option[LoadListing.LoadListing])
    extends Identifiable

object WorkflowStepInput {
  def apply(step: WorkflowStepInputImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowStepInput = {
    WorkflowStepInput(
        translateOptional(step.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
        translateOptional(step.getLabel),
        translateOptionalArray(step.getSource).map(source =>
          Identifier.parse(source.toString, stripFragPrefix, defaultNamespace)
        ),
        translateOptional(step.getDefault).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(step.getValueFrom).map(CwlValue(_, schemaDefs)),
        translateOptional(step.getLinkMerge).map(LinkMergeMethod.from),
        translateOptional(step.getPickValue).map(PickValueMethod.from),
        translateOptional(step.getLoadContents).map(_.booleanValue()),
        translateOptional(step.getLoadListing).map(LoadListing.from)
    )
  }

  def applyArray(steps: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowStepInput] = {
    steps.asScala.toVector.map {
      case param: WorkflowStepInputImpl =>
        WorkflowStepInput(param, schemaDefs, stripFragPrefix, defaultNamespace)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowStepInput value ${other}")
    }
  }
}

case class WorkflowStepOutput(id: Option[Identifier]) extends Identifiable

object WorkflowStepOutput {
  def apply(step: WorkflowStepOutputImpl,
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowStepOutput = {
    WorkflowStepOutput(
        translateOptional(step.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace))
    )
  }

  def applyArray(steps: java.util.List[java.lang.Object],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowStepOutput] = {
    steps.asScala.toVector.map {
      case param: WorkflowStepOutputImpl =>
        WorkflowStepOutput(param, stripFragPrefix, defaultNamespace)
      case param: String =>
        WorkflowStepOutput(Some(Identifier.parse(param, stripFragPrefix, defaultNamespace)))
      case other =>
        throw new RuntimeException(
            s"unexpected WorkflowStepOutput value ${other}"
        )
    }
  }
}

case class WorkflowStep(id: Option[Identifier],
                        label: Option[String],
                        doc: Option[String],
                        inputs: Vector[WorkflowStepInput],
                        outputs: Vector[WorkflowStepOutput],
                        run: Process,
                        when: Option[CwlValue],
                        scatter: Vector[String],
                        scatterMethod: Option[ScatterMethod.ScatterMethod],
                        requirements: Vector[Requirement],
                        hints: Vector[Hint])
    extends Identifiable

object WorkflowStep {
  def apply(step: WorkflowStepImpl, ctx: Parser): WorkflowStep = {
    parse(step, ctx)._1
  }

  def parse(step: WorkflowStepImpl,
            ctx: Parser,
            dependencies: Document = Document.empty,
            rawProcesses: Map[Identifier, java.lang.Object] = Map.empty,
            isGraph: Boolean = false,
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): (WorkflowStep, Document) = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(step.getRequirements, ctx.schemaDefs)
    val id = translateOptional(step.getId).map(Identifier.parse(_, stripFragPrefix))
    val (runProcess, newDoc) = step.getRun match {
      case process: ProcessInterface =>
        val defaultFrag = id.flatMap(_.frag.map(f => s"${f}/run"))
        ctx.parse(process,
                  defaultNamespace = defaultNamespace,
                  defaultFrag = defaultFrag,
                  dependencies = dependencies)
      case uri: String if isGraph =>
        val runId = Identifier.parse(uri, defaultNamespace = defaultNamespace)
        println(dependencies.keys)
        println(rawProcesses.keys)
        if (dependencies.contains(runId)) {
          (dependencies(runId), dependencies)
        } else if (rawProcesses.contains(runId)) {
          ctx.parse(rawProcesses(runId),
                    dependencies = dependencies,
                    rawProcesses = rawProcesses,
                    isGraph = true)
        } else {
          throw new Exception(s"invalid process ${runId}")
        }
      case path: String => ctx.parseImport(path, dependencies)
      case other =>
        throw new RuntimeException(s"unexpected run value ${other} for step ${step}")
    }
    val wfStep = WorkflowStep(
        id,
        translateOptional(step.getLabel),
        translateDoc(step.getDoc),
        WorkflowStepInput.applyArray(step.getIn, allSchemaDefs, stripFragPrefix),
        WorkflowStepOutput.applyArray(step.getOut, stripFragPrefix),
        runProcess,
        translateOptional(step.getWhen).map(CwlValue(_, allSchemaDefs)),
        translateOptionalArray(step.getScatter).map(_.toString),
        translateOptional(step.getScatterMethod).map(ScatterMethod.from),
        requirements,
        Requirement.applyHints(step.getHints, allSchemaDefs, ctx.hintSchemas)
    )
    (wfStep, newDoc)
  }

  def parseArray(steps: java.util.List[java.lang.Object],
                 ctx: Parser,
                 dependencies: Document = Document.empty,
                 rawProcesses: Map[Identifier, java.lang.Object] = Map.empty,
                 isGraph: Boolean = false,
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): (Vector[WorkflowStep], Document) = {
    steps.asScala.toVector.foldLeft(Vector.empty[WorkflowStep], dependencies) {
      case ((stepAccu, docAccu), rawStep: WorkflowStepImpl) =>
        val (step, newDoc) =
          WorkflowStep.parse(rawStep,
                             ctx,
                             docAccu,
                             rawProcesses,
                             isGraph,
                             stripFragPrefix,
                             defaultNamespace)
        (stepAccu :+ step, newDoc)
      case (_, other) =>
        throw new RuntimeException(s"unexpected WorkflowStep value ${other}")
    }
  }
}
case class Workflow(source: Option[String],
                    cwlVersion: Option[CWLVersion],
                    id: Option[Identifier],
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
            ctx: Parser,
            source: Option[Path] = None,
            defaultFrag: Option[String] = None): Workflow = {
    parse(workflow, ctx, source, defaultFrag = defaultFrag)._1
  }

  def parse(workflow: WorkflowImpl,
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            dependencies: Document = Document.empty,
            rawProcesses: Map[Identifier, Object] = Map.empty,
            isGraph: Boolean = false): (Workflow, Document) = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(workflow.getRequirements, ctx.schemaDefs)
    val newContext = ctx.copy(schemaDefs = allSchemaDefs)
    val id = Identifier.get(workflow.getId, defaultNamespace, defaultFrag, source)
    val stripFragPrefix = if (isGraph) id.flatMap(_.frag.map(p => s"${p}/")) else None
    val (steps, newDependencies) =
      WorkflowStep.parseArray(workflow.getSteps,
                              newContext,
                              dependencies,
                              rawProcesses,
                              isGraph,
                              stripFragPrefix,
                              defaultNamespace)
    val wf = Workflow(
        source.map(_.toString),
        translateOptional(workflow.getCwlVersion),
        id,
        translateOptional(workflow.getLabel),
        translateDoc(workflow.getDoc),
        translateOptionalArray(workflow.getIntent).map(translateString),
        WorkflowInputParameter.applyArray(workflow.getInputs, allSchemaDefs, stripFragPrefix),
        WorkflowOutputParameter.applyArray(workflow.getOutputs, allSchemaDefs, stripFragPrefix),
        steps,
        requirements,
        Requirement.applyHints(workflow.getHints, allSchemaDefs, ctx.hintSchemas)
    )
    (wf, newDependencies.addProcess(wf))
  }
}

case class ExpressionToolOutputParameter(id: Option[Identifier],
                                         label: Option[String],
                                         doc: Option[String],
                                         cwlType: CwlType,
                                         secondaryFiles: Vector[SecondaryFile],
                                         format: Option[CwlValue],
                                         streamable: Option[Boolean])
    extends OutputParameter

object ExpressionToolOutputParameter {
  def apply(param: ExpressionToolOutputParameterImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): ExpressionToolOutputParameter = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    ExpressionToolOutputParameter(
        translateOptional(param.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue())
    )
  }

  def applyArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[ExpressionToolOutputParameter] = {
    params.asScala.toVector.map {
      case param: ExpressionToolOutputParameterImpl =>
        ExpressionToolOutputParameter(param, schemaDefs, stripFragPrefix, defaultNamespace)
      case other =>
        throw new RuntimeException(s"unexpected OperationInputParameter value ${other}")
    }
  }
}

case class ExpressionTool(source: Option[String],
                          cwlVersion: Option[CWLVersion],
                          id: Option[Identifier],
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
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            isGraph: Boolean = false): ExpressionTool = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(expressionTool.getRequirements, ctx.schemaDefs)
    val id = Identifier.get(expressionTool.getId, defaultNamespace, defaultFrag, source)
    val stripFragPrefix = if (isGraph) id.flatMap(_.frag.map(p => s"${p}/")) else None
    ExpressionTool(
        source.map(_.toString),
        translateOptional(expressionTool.getCwlVersion),
        id,
        translateOptional(expressionTool.getLabel),
        translateDoc(expressionTool.getDoc),
        translateOptionalArray(expressionTool.getIntent).map(translateString),
        WorkflowInputParameter.applyArray(expressionTool.getInputs,
                                          allSchemaDefs,
                                          stripFragPrefix,
                                          defaultNamespace),
        ExpressionToolOutputParameter.applyArray(expressionTool.getOutputs,
                                                 allSchemaDefs,
                                                 stripFragPrefix,
                                                 defaultNamespace),
        CwlValue(expressionTool.getExpression, allSchemaDefs),
        requirements,
        Requirement.applyHints(expressionTool.getHints, allSchemaDefs, ctx.hintSchemas)
    )
  }
}

case class OperationInputParameter(id: Option[Identifier],
                                   label: Option[String],
                                   doc: Option[String],
                                   cwlType: CwlType,
                                   default: Option[CwlValue],
                                   secondaryFiles: Vector[SecondaryFile],
                                   format: Vector[CwlValue],
                                   streamable: Option[Boolean],
                                   loadContents: Option[Boolean],
                                   loadListing: Option[LoadListing.LoadListing])
    extends InputParameter

object OperationInputParameter {
  def apply(param: OperationInputParameterImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): OperationInputParameter = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    OperationInputParameter(
        translateOptional(param.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
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
      schemaDefs: Map[String, CwlSchema] = Map.empty,
      stripFragPrefix: Option[String] = None
  ): Vector[OperationInputParameter] = {
    params.asScala.toVector.map {
      case param: OperationInputParameterImpl =>
        OperationInputParameter(param, schemaDefs, stripFragPrefix)
      case other =>
        throw new RuntimeException(s"unexpected OperationInputParameter value ${other}")
    }
  }
}

case class OperationOutputParameter(id: Option[Identifier],
                                    label: Option[String],
                                    doc: Option[String],
                                    cwlType: CwlType,
                                    secondaryFiles: Vector[SecondaryFile],
                                    format: Option[CwlValue],
                                    streamable: Option[Boolean])
    extends OutputParameter

object OperationOutputParameter {
  def apply(param: OperationOutputParameterImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): OperationOutputParameter = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    OperationOutputParameter(
        translateOptional(param.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
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
      schemaDefs: Map[String, CwlSchema] = Map.empty,
      stripFragPrefix: Option[String] = None
  ): Vector[OperationOutputParameter] = {
    params.asScala.toVector.map {
      case param: OperationOutputParameterImpl =>
        OperationOutputParameter(param, schemaDefs, stripFragPrefix)
      case other =>
        throw new RuntimeException(s"unexpected OperationOutputParameter value ${other}")
    }
  }
}

case class Operation(source: Option[String],
                     cwlVersion: Option[CWLVersion],
                     id: Option[Identifier],
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
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            isGraph: Boolean = false): Operation = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(operation.getRequirements, ctx.schemaDefs)
    val id = Identifier.get(operation.getId, defaultNamespace, defaultFrag, source)
    val stripFragPrefix = if (isGraph) id.flatMap(_.frag.map(p => s"${p}/")) else None
    Operation(
        source.map(_.toString),
        translateOptional(operation.getCwlVersion),
        id,
        translateOptional(operation.getLabel),
        translateDoc(operation.getDoc),
        translateOptionalArray(operation.getIntent).map(translateString),
        OperationInputParameter.applyArray(operation.getInputs, allSchemaDefs, stripFragPrefix),
        OperationOutputParameter.applyArray(operation.getOutputs, allSchemaDefs, stripFragPrefix),
        requirements,
        Requirement.applyHints(operation.getHints, allSchemaDefs, ctx.hintSchemas)
    )
  }
}
