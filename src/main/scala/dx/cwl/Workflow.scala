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
  def parse(binding: InputBindingImpl): WorkflowInputBinding = {
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
                                  streamable: Boolean,
                                  loadContents: Boolean,
                                  loadListing: LoadListing.LoadListing)
    extends InputParameter
    with Loadable

object WorkflowInputParameter {
  def parse(param: WorkflowInputParameterImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowInputParameter = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    assert(stdfile.isEmpty)
    val inputBinding = translateOptional(param.getInputBinding).map {
      case binding: InputBindingImpl => WorkflowInputBinding.parse(binding)
    }
    WorkflowInputParameter(
        translateOptional(param.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getDefault).map(CwlValue(_, schemaDefs)),
        inputBinding,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = true),
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).exists(_.booleanValue()),
        translateOptional(param.getLoadContents).exists(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from).getOrElse(LoadListing.No)
    )
  }

  def parseArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowInputParameter] = {
    params.asScala.toVector.map {
      case param: WorkflowInputParameterImpl =>
        WorkflowInputParameter.parse(param, schemaDefs, stripPrefix, defaultNamespace)
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

trait Sink {
  val sources: Vector[Identifier]
  val linkMerge: Option[LinkMergeMethod.LinkMergeMethod]
  val pickValue: Option[PickValueMethod.PickValueMethod]
}

case class WorkflowOutputParameter(id: Option[Identifier],
                                   label: Option[String],
                                   doc: Option[String],
                                   cwlType: CwlType,
                                   secondaryFiles: Vector[SecondaryFile],
                                   format: Option[CwlValue],
                                   streamable: Boolean,
                                   sources: Vector[Identifier],
                                   linkMerge: Option[LinkMergeMethod.LinkMergeMethod],
                                   pickValue: Option[PickValueMethod.PickValueMethod])
    extends OutputParameter
    with Sink

object WorkflowOutputParameter {
  def parse(param: WorkflowOutputParameterImpl,
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
          if (id.isDefined) {
            val prefix = s"${id.get.frag}/"
            if (src.frag.startsWith(prefix)) {
              src.copy(frag = src.frag.drop(prefix.length))
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
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = false),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).exists(_.booleanValue()),
        sources,
        translateOptional(param.getLinkMerge)
          .map(LinkMergeMethod.from)
          .orElse(Option.when(sources.size > 1)(LinkMergeMethod.MergeNested)),
        translateOptional(param.getPickValue).map(PickValueMethod.from)
    )
  }

  def parseArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowOutputParameter] = {
    params.asScala.toVector.map {
      case param: WorkflowOutputParameterImpl =>
        WorkflowOutputParameter.parse(param, schemaDefs, stripFragPrefix, defaultNamespace)
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
                             sources: Vector[Identifier],
                             default: Option[CwlValue],
                             valueFrom: Option[CwlValue],
                             linkMerge: Option[LinkMergeMethod.LinkMergeMethod],
                             pickValue: Option[PickValueMethod.PickValueMethod],
                             loadContents: Boolean,
                             loadListing: LoadListing.LoadListing)
    extends Identifiable
    with Sink
    with Loadable

object WorkflowStepInput {
  def parse(step: WorkflowStepInputImpl,
            schemaDefs: Map[String, CwlSchema],
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowStepInput = {
    val sources = translateOptionalArray(step.getSource).map { source =>
      Identifier.parse(source.toString, stripFragPrefix, defaultNamespace)
    }
    WorkflowStepInput(
        translateOptional(step.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace)),
        translateOptional(step.getLabel),
        sources,
        translateOptional(step.getDefault).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(step.getValueFrom).map(CwlValue(_, schemaDefs)),
        translateOptional(step.getLinkMerge)
          .map(LinkMergeMethod.from)
          .orElse(Option.when(sources.size > 1)(LinkMergeMethod.MergeNested)),
        translateOptional(step.getPickValue).map(PickValueMethod.from),
        translateOptional(step.getLoadContents).exists(_.booleanValue()),
        translateOptional(step.getLoadListing).map(LoadListing.from).getOrElse(LoadListing.No)
    )
  }

  def parseArray(steps: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowStepInput] = {
    steps.asScala.toVector.map {
      case param: WorkflowStepInputImpl =>
        WorkflowStepInput.parse(param, schemaDefs, stripFragPrefix, defaultNamespace)
      case other =>
        throw new RuntimeException(s"unexpected WorkflowStepInput value ${other}")
    }
  }
}

case class WorkflowStepOutput(id: Option[Identifier]) extends Identifiable

object WorkflowStepOutput {
  def parse(step: WorkflowStepOutputImpl,
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None): WorkflowStepOutput = {
    WorkflowStepOutput(
        translateOptional(step.getId).map(Identifier.parse(_, stripFragPrefix, defaultNamespace))
    )
  }

  def parseArray(steps: java.util.List[java.lang.Object],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[WorkflowStepOutput] = {
    steps.asScala.toVector.map {
      case param: WorkflowStepOutputImpl =>
        WorkflowStepOutput.parse(param, stripFragPrefix, defaultNamespace)
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
                        scatter: Vector[Identifier],
                        scatterMethod: Option[ScatterMethod.ScatterMethod],
                        requirements: Vector[Requirement],
                        hints: Vector[Hint])
    extends Identifiable

object WorkflowStep {
  def parse(step: WorkflowStepImpl,
            ctx: Parser,
            dependencies: Document = Document.empty,
            rawProcesses: Map[Identifier, java.lang.Object] = Map.empty,
            isGraph: Boolean = false,
            stripFragPrefix: Option[String] = None,
            defaultNamespace: Option[String] = None,
            simplifyProcessAutoIds: Boolean = false): (WorkflowStep, Document) = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(step.getRequirements, ctx.schemaDefs)
    val id = translateOptional(step.getId).map(Identifier.parse(_, stripFragPrefix))
    val runResult = step.getRun match {
      case process: ProcessInterface =>
        val defaultFrag = id.map(i => s"${i.frag}/run")
        ctx.parse(process,
                  defaultNamespace = defaultNamespace,
                  defaultFrag = defaultFrag,
                  dependencies = dependencies,
                  simplifyProcessAutoIds = simplifyProcessAutoIds)
      case uri: String if isGraph =>
        val runId = Identifier.parse(uri,
                                     defaultNamespace = defaultNamespace,
                                     simplifyFrag = simplifyProcessAutoIds)
        if (dependencies.contains(runId)) {
          ParserResult(Some(dependencies(runId)), dependencies)
        } else if (rawProcesses.contains(runId)) {
          ctx.parse(rawProcesses(runId),
                    dependencies = dependencies,
                    rawProcesses = rawProcesses,
                    isGraph = true,
                    simplifyProcessAutoIds = simplifyProcessAutoIds)
        } else {
          throw new Exception(s"invalid process ${runId}")
        }
      case path: String =>
        ctx.parseImport(path, dependencies, simplifyProcessAutoIds)
      case other =>
        throw new RuntimeException(s"unexpected run value ${other} for step ${step}")
    }
    val wfStep = WorkflowStep(
        id,
        translateOptional(step.getLabel),
        translateDoc(step.getDoc),
        WorkflowStepInput.parseArray(step.getIn, allSchemaDefs, stripFragPrefix),
        WorkflowStepOutput.parseArray(step.getOut, stripFragPrefix),
        runResult.mainProcess.get,
        translateOptional(step.getWhen).map(CwlValue(_, allSchemaDefs)),
        translateOptionalArray(step.getScatter).map(source =>
          Identifier.parse(source.toString, stripFragPrefix, defaultNamespace)
        ),
        translateOptional(step.getScatterMethod).map(ScatterMethod.from),
        requirements,
        Requirement.applyHints(step.getHints, allSchemaDefs, ctx.hintSchemas)
    )
    (wfStep, runResult.document)
  }

  def parseArray(steps: java.util.List[java.lang.Object],
                 ctx: Parser,
                 dependencies: Document = Document.empty,
                 rawProcesses: Map[Identifier, java.lang.Object] = Map.empty,
                 isGraph: Boolean = false,
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None,
                 simplifyProcessAutoIds: Boolean = false): (Vector[WorkflowStep], Document) = {
    steps.asScala.toVector.foldLeft(Vector.empty[WorkflowStep], dependencies) {
      case ((stepAccu, docAccu), rawStep: WorkflowStepImpl) =>
        val (step, newDoc) =
          WorkflowStep.parse(rawStep,
                             ctx,
                             docAccu,
                             rawProcesses,
                             isGraph,
                             stripFragPrefix,
                             defaultNamespace,
                             simplifyProcessAutoIds)
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
  def parse(workflow: WorkflowImpl,
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            dependencies: Document = Document.empty,
            rawProcesses: Map[Identifier, Object] = Map.empty,
            isGraph: Boolean = false,
            mainId: Identifier = Identifier.MainId,
            simplifyProcessAutoIds: Boolean = false): (Workflow, Document) = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(workflow.getRequirements, ctx.schemaDefs)
    val newContext = ctx.copy(schemaDefs = allSchemaDefs)
    val rawId = Identifier.get(workflow.getId,
                               defaultNamespace,
                               defaultFrag,
                               source,
                               simplifyFrag = simplifyProcessAutoIds)
    val stripFragPrefix = if (isGraph) rawId.map(i => s"${i.frag}/") else None
    val (steps, newDependencies) =
      WorkflowStep.parseArray(workflow.getSteps,
                              newContext,
                              dependencies,
                              rawProcesses,
                              isGraph,
                              stripFragPrefix,
                              defaultNamespace,
                              simplifyProcessAutoIds)
    val wfId = Option
      .when(isGraph && rawId.contains(mainId)) {
        val namespace = rawId.map(_.namespace).getOrElse(defaultNamespace)
        defaultFrag
          .map(frag => Identifier(namespace, frag))
          .orElse(
              Option.when(source.isDefined)(Identifier.fromSource(source.get, namespace))
          )
          .map { wfId =>
            if (newDependencies.contains(wfId)) {
              // the document already has a process with the given ID - make it unique by adding a suffix
              Iterator
                .from(1)
                .map(i => wfId.copy(frag = s"${wfId.frag}-${i}"))
                .collectFirst {
                  case id if !newDependencies.contains(id) => id
                }
                .get
            } else {
              wfId
            }
          }
      }
      .flatten
      .orElse(rawId)
    val wf = Workflow(
        source.map(_.toString),
        translateOptional(workflow.getCwlVersion),
        wfId,
        translateOptional(workflow.getLabel),
        translateDoc(workflow.getDoc),
        translateOptionalArray(workflow.getIntent).map(translateString),
        WorkflowInputParameter.parseArray(workflow.getInputs, allSchemaDefs, stripFragPrefix),
        WorkflowOutputParameter.parseArray(workflow.getOutputs, allSchemaDefs, stripFragPrefix),
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
                                         streamable: Boolean)
    extends OutputParameter

object ExpressionToolOutputParameter {
  def parse(param: ExpressionToolOutputParameterImpl,
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
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = false),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).exists(_.booleanValue())
    )
  }

  def parseArray(params: java.util.List[java.lang.Object],
                 schemaDefs: Map[String, CwlSchema],
                 stripFragPrefix: Option[String] = None,
                 defaultNamespace: Option[String] = None): Vector[ExpressionToolOutputParameter] = {
    params.asScala.toVector.map {
      case param: ExpressionToolOutputParameterImpl =>
        ExpressionToolOutputParameter.parse(param, schemaDefs, stripFragPrefix, defaultNamespace)
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
  def parse(expressionTool: ExpressionToolImpl,
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            isGraph: Boolean = false,
            mainId: Identifier = Identifier.MainId,
            simplifyProcessAutoIds: Boolean = false): ExpressionTool = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(expressionTool.getRequirements, ctx.schemaDefs)
    val rawId = Identifier.get(expressionTool.getId,
                               defaultNamespace,
                               defaultFrag,
                               source,
                               simplifyFrag = simplifyProcessAutoIds)
    val stripFragPrefix = if (isGraph) rawId.map(i => s"${i.frag}/") else None
    val toolId = Option
      .when(isGraph && rawId.contains(mainId)) {
        val namespace = rawId.map(_.namespace).getOrElse(defaultNamespace)
        defaultFrag
          .map(frag => Identifier(namespace, frag))
          .orElse(
              Option.when(source.isDefined)(Identifier.fromSource(source.get, namespace))
          )
      }
      .flatten
      .orElse(rawId)
    ExpressionTool(
        source.map(_.toString),
        translateOptional(expressionTool.getCwlVersion),
        toolId,
        translateOptional(expressionTool.getLabel),
        translateDoc(expressionTool.getDoc),
        translateOptionalArray(expressionTool.getIntent).map(translateString),
        WorkflowInputParameter.parseArray(expressionTool.getInputs,
                                          allSchemaDefs,
                                          stripFragPrefix,
                                          defaultNamespace),
        ExpressionToolOutputParameter.parseArray(expressionTool.getOutputs,
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
                                   streamable: Boolean,
                                   loadContents: Boolean,
                                   loadListing: LoadListing.LoadListing)
    extends InputParameter
    with Loadable

object OperationInputParameter {
  def parse(param: OperationInputParameterImpl,
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
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = true),
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).exists(_.booleanValue()),
        translateOptional(param.getLoadContents).exists(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from).getOrElse(LoadListing.No)
    )
  }

  def parseArray(
      params: java.util.List[java.lang.Object],
      schemaDefs: Map[String, CwlSchema] = Map.empty,
      stripFragPrefix: Option[String] = None
  ): Vector[OperationInputParameter] = {
    params.asScala.toVector.map {
      case param: OperationInputParameterImpl =>
        OperationInputParameter.parse(param, schemaDefs, stripFragPrefix)
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
                                    streamable: Boolean)
    extends OutputParameter

object OperationOutputParameter {
  def parse(param: OperationOutputParameterImpl,
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
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = false),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).exists(_.booleanValue())
    )
  }

  def parseArray(
      params: java.util.List[java.lang.Object],
      schemaDefs: Map[String, CwlSchema] = Map.empty,
      stripFragPrefix: Option[String] = None
  ): Vector[OperationOutputParameter] = {
    params.asScala.toVector.map {
      case param: OperationOutputParameterImpl =>
        OperationOutputParameter.parse(param, schemaDefs, stripFragPrefix)
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
  def parse(operation: OperationImpl,
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            isGraph: Boolean = false,
            mainId: Identifier = Identifier.MainId,
            simplifyProcessAutoIds: Boolean = false): Operation = {
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(operation.getRequirements, ctx.schemaDefs)
    val rawId = Identifier.get(operation.getId,
                               defaultNamespace,
                               defaultFrag,
                               source,
                               simplifyFrag = simplifyProcessAutoIds)
    val stripFragPrefix = if (isGraph) rawId.map(i => s"${i.frag}/") else None
    val opId = Option
      .when(isGraph && rawId.contains(mainId)) {
        val namespace = rawId.map(_.namespace).getOrElse(defaultNamespace)
        defaultFrag
          .map(frag => Identifier(namespace, frag))
          .orElse(
              Option.when(source.isDefined)(Identifier.fromSource(source.get, namespace))
          )
      }
      .flatten
      .orElse(rawId)
    Operation(
        source.map(_.toString),
        translateOptional(operation.getCwlVersion),
        opId,
        translateOptional(operation.getLabel),
        translateDoc(operation.getDoc),
        translateOptionalArray(operation.getIntent).map(translateString),
        OperationInputParameter.parseArray(operation.getInputs, allSchemaDefs, stripFragPrefix),
        OperationOutputParameter.parseArray(operation.getOutputs, allSchemaDefs, stripFragPrefix),
        requirements,
        Requirement.applyHints(operation.getHints, allSchemaDefs, ctx.hintSchemas)
    )
  }
}
