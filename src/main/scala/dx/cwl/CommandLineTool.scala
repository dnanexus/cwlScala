package dx.cwl

import java.nio.file.Path
import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{
  CWLVersion,
  CommandInputParameterImpl,
  CommandLineBindingImpl,
  CommandLineToolImpl,
  CommandOutputBindingImpl,
  CommandOutputParameterImpl
}

import scala.jdk.CollectionConverters._

// https://www.commonwl.org/v1.2/CommandLineTool.html#stdin
// https://www.commonwl.org/v1.2/CommandLineTool.html#stdout
// https://www.commonwl.org/v1.2/CommandLineTool.html#stderr
object StdFile extends Enumeration {
  type StdFile = Value
  val Stdin, Stdout, Stderr = Value
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandLineBinding
case class CommandInputBinding(position: CwlValue,
                               prefix: Option[String],
                               separate: Boolean,
                               itemSeparator: Option[String],
                               shellQuote: Boolean,
                               valueFrom: Option[CwlValue])

object CommandInputBinding {
  def parse(binding: CommandLineBindingImpl,
            schemaDefs: Map[String, CwlSchema]): CommandInputBinding = {
    CommandInputBinding(
        translateOptionalObject(binding.getPosition)
          .map(CwlValue(_, schemaDefs))
          .getOrElse(IntValue(0)),
        translateOptional(binding.getPrefix),
        translateOptional(binding.getSeparate).forall(_.booleanValue()),
        translateOptional(binding.getItemSeparator),
        translateOptional(binding.getShellQuote).forall(_.booleanValue()),
        translateOptionalObject(binding.getValueFrom).map(CwlValue(_, schemaDefs))
    )
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandInputParameter
case class CommandInputParameter(id: Option[Identifier],
                                 label: Option[String],
                                 doc: Option[String],
                                 cwlType: CwlType,
                                 default: Option[CwlValue],
                                 inputBinding: Option[CommandInputBinding],
                                 secondaryFiles: Vector[SecondaryFile],
                                 format: Vector[CwlValue],
                                 streamable: Boolean,
                                 loadContents: Boolean,
                                 loadListing: LoadListing.LoadListing)
    extends InputParameter
    with Loadable {
  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CommandInputParameter = {
    copy(
        id = id.map(
            _.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)
        ),
        cwlType = CwlType.copySimplifyIds(cwlType,
                                          dropNamespace,
                                          replacePrefix,
                                          simplifyAutoNames,
                                          dropCwlExtension)
    )
  }
}

object CommandInputParameter {
  def parse(
      param: CommandInputParameterImpl,
      schemaDefs: Map[String, CwlSchema],
      defaultNamespace: Option[String] = None
  ): (CommandInputParameter, Boolean) = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    val inparam = CommandInputParameter(
        translateOptional(param.getId)
          .map(Identifier.parse(_, defaultNamespace)),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptionalObject(param.getDefault).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding.parse(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        },
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = true),
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).exists(_.booleanValue()),
        translateOptional(param.getLoadContents).exists(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from).getOrElse(LoadListing.No)
    )
    (inparam, stdfile.contains(StdFile.Stdin))
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandOutputBinding
case class CommandOutputBinding(glob: Vector[CwlValue],
                                outputEval: Option[CwlValue],
                                loadContents: Boolean,
                                loadListing: LoadListing.LoadListing)
    extends Loadable

object CommandOutputBinding {
  def parse(binding: CommandOutputBindingImpl,
            schemaDefs: Map[String, CwlSchema]): CommandOutputBinding = {
    CommandOutputBinding(
        translateOptionalArray(binding.getGlob).map(CwlValue(_, schemaDefs)),
        translateOptional(binding.getOutputEval).map(CwlValue(_, schemaDefs)),
        translateOptional(binding.getLoadContents).exists(_.booleanValue()),
        translateOptional(binding.getLoadListing).map(LoadListing.from).getOrElse(LoadListing.No)
    )
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandOutputParameter
case class CommandOutputParameter(id: Option[Identifier],
                                  label: Option[String],
                                  doc: Option[String],
                                  cwlType: CwlType,
                                  outputBinding: Option[CommandOutputBinding],
                                  secondaryFiles: Vector[SecondaryFile],
                                  format: Option[CwlValue],
                                  streamable: Boolean)
    extends OutputParameter {
  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CommandOutputParameter = {
    copy(
        id = id.map(
            _.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)
        ),
        cwlType = CwlType.copySimplifyIds(cwlType,
                                          dropNamespace,
                                          replacePrefix,
                                          simplifyAutoNames,
                                          dropCwlExtension)
    )
  }
}

object CommandOutputParameter {
  def parse(
      param: CommandOutputParameterImpl,
      schemaDefs: Map[String, CwlSchema],
      defaultNamespace: Option[String] = None
  ): (CommandOutputParameter, Option[StdFile.StdFile]) = {
    val (types, stdfile) = CwlType.translate(param.getType, schemaDefs)
    val outputBinding = translateOptional(param.getOutputBinding) match {
      case Some(_) if stdfile.nonEmpty =>
        throw new RuntimeException(s"outputBinding not allowed for type ${stdfile.get}")
      case Some(binding: CommandOutputBindingImpl) =>
        Some(CommandOutputBinding.parse(binding, schemaDefs))
      case None if stdfile.nonEmpty =>
        Some(
            CommandOutputBinding(glob = Vector(RandomFile(stdfile.get)),
                                 outputEval = None,
                                 loadContents = false,
                                 loadListing = LoadListing.No)
        )
      case None => None
      case other =>
        throw new RuntimeException(s"unexpected CommandOutputBinding value ${other}")
    }
    val streamable = if (stdfile.nonEmpty) {
      true
    } else {
      translateOptional(param.getStreamable).exists(_.booleanValue())
    }
    val outparam = CommandOutputParameter(
        translateOptional(param.getId)
          .map(Identifier.parse(_, defaultNamespace)),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        outputBinding,
        SecondaryFile.applyArray(param.getSecondaryFiles, schemaDefs, isInput = false),
        translateOptionalObject(param.getFormat).map(CwlValue(_, schemaDefs)),
        streamable
    )
    (outparam, stdfile)
  }
}

sealed trait Argument
case class ExprArgument(expr: CwlValue) extends Argument
case class BindingArgument(binding: CommandInputBinding) extends Argument

/**
  * @note
  * - If `successCodes` is not specified, it is initialized to `Set(0)`
  * @see https://www.commonwl.org/v1.2/CommandLineTool.html#CommandOutputBinding
  */
case class CommandLineTool(source: Option[String],
                           cwlVersion: Option[CWLVersion],
                           id: Option[Identifier],
                           label: Option[String],
                           doc: Option[String],
                           intent: Vector[String],
                           inputs: Vector[CommandInputParameter],
                           outputs: Vector[CommandOutputParameter],
                           baseCommand: Vector[String],
                           arguments: Vector[Argument],
                           stdin: Option[CwlValue],
                           stdout: Option[CwlValue],
                           stderr: Option[CwlValue],
                           requirements: Vector[Requirement],
                           hints: Vector[Hint],
                           successCodes: Set[Int],
                           temporaryFailCodes: Set[Int],
                           permanentFailCodes: Set[Int])
    extends Process {
  override def copySimplifyIds(
      dropNamespace: Boolean,
      replacePrefix: (Either[Boolean, String], Option[String]),
      simplifyAutoNames: Boolean,
      dropCwlExtension: Boolean
  ): CommandLineTool = {
    val (simplifiedId, childReplacePrefix) =
      getIdAndChildReplacePrefix(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)
    copy(
        id = simplifiedId,
        inputs = inputs.map(
            _.copySimplifyIds(dropNamespace,
                              childReplacePrefix,
                              simplifyAutoNames,
                              dropCwlExtension)
        ),
        outputs = outputs.map(
            _.copySimplifyIds(dropNamespace,
                              childReplacePrefix,
                              simplifyAutoNames,
                              dropCwlExtension)
        )
    )
  }
}

object CommandLineTool {

  /**
    * Creates a [[CommandLineTool]] from the [[CommandLineToolImpl]] created by the Java parser.
    * @param tool the Java object.
    * @param ctx the Parser.
    * @param source the CWL original source code.
    * @param defaultNamespace the namespace to use when creating Identifiers when one is not present.
    * @param defaultMainFrag an optional tool name, to use if it is not specified in the document.
    * @return a [[CommandLineTool]]
    */
  def parse(tool: CommandLineToolImpl,
            ctx: Parser,
            source: Option[Path] = None,
            defaultNamespace: Option[String],
            defaultMainFrag: Option[String] = None): CommandLineTool = {
    val rawId = Identifier.get(tool.getId, defaultNamespace, defaultMainFrag, source)
    val toolId = rawId.orElse {
      val namespace = rawId.map(_.namespace).getOrElse(defaultNamespace)
      defaultMainFrag
        .map(frag => Identifier(namespace, frag))
        .orElse(
            Option.when(source.isDefined)(Identifier.fromSource(source.get, namespace))
        )
    }
    val (requirements, allSchemaDefs) =
      Requirement.applyRequirements(tool.getRequirements, ctx.schemaDefs)
    // An input may have type `stdin`, which is a file that is created from the
    // standard input piped to the CommandLineTool. A maximum of one input parameter
    // can have the `stdin` type, and if there is one then the tool's `stdin`
    // attribute cannot be set.
    // https://www.commonwl.org/v1.2/CommandLineTool.html#stdin
    val (inputParams, isStdin) = tool.getInputs.asScala
      .map {
        case param: CommandInputParameterImpl =>
          CommandInputParameter.parse(param, allSchemaDefs, defaultNamespace)
        case other =>
          throw new RuntimeException(s"unexpected CommandInputParameter value ${other}")
      }
      .toVector
      .unzip
    val stdinIndexes = isStdin.zipWithIndex.collect {
      case (b, i) if b => i
    }
    if (stdinIndexes.size > 1) {
      throw new RuntimeException("more than one parameter specified 'stdin'")
    }
    val paramStdin: Option[CommandInputParameter] = stdinIndexes.headOption.map(inputParams(_))
    val toolStdin: Option[CwlValue] =
      translateOptionalObject(tool.getStdin).map(CwlValue(_, allSchemaDefs))
    if (paramStdin.isDefined && toolStdin.isDefined) {
      throw new RuntimeException(
          s"'stdin' specified at both the tool level and by parameter ${paramStdin.get}"
      )
    }
    val stdin = paramStdin
      .map { param =>
        val paramId = param.id
          .getOrElse(
              throw new Exception(s"missing input parameter ${paramStdin}")
          )
          .frag
        CwlValue(s"$${inputs.${paramId}.path}", allSchemaDefs)
      }
      .orElse(toolStdin)
    // An output parameter may have type `stdout` or `stderr`. There can be a maximum of
    // one output parameter with each type, and if there is one then the corresponding
    // CommandLineTool attribute cannot also be set.
    // https://www.commonwl.org/v1.2/CommandLineTool.html#stdout
    val (outputParams, stdfile) = tool.getOutputs.asScala
      .map {
        case param: CommandOutputParameterImpl =>
          CommandOutputParameter.parse(param, allSchemaDefs, defaultNamespace)
        case other =>
          throw new RuntimeException(s"unexpected CommandOutputParameter value ${other}")
      }
      .toVector
      .unzip
    val stdout: Option[CwlValue] =
      translateOptionalObject(tool.getStdout).map(CwlValue(_, allSchemaDefs)) match {
        case None if stdfile.contains(Some(StdFile.Stdout)) =>
          Some(RandomFile(StdFile.Stdout))
        case other => other
      }
    val stderr: Option[CwlValue] =
      translateOptionalObject(tool.getStderr).map(CwlValue(_, allSchemaDefs)) match {
        case None if stdfile.contains(Some(StdFile.Stderr)) =>
          Some(RandomFile(StdFile.Stderr))
        case other => other
      }
    val arguments = translateOptionalArray(tool.getArguments).map {
      case binding: CommandLineBindingImpl =>
        BindingArgument(CommandInputBinding.parse(binding, allSchemaDefs))
      case expr =>
        ExprArgument(CwlValue(expr, allSchemaDefs))
    }
    CommandLineTool(
        source.map(_.toString),
        translateOptional(tool.getCwlVersion),
        toolId,
        translateOptional(tool.getLabel),
        translateDoc(tool.getDoc),
        translateOptionalArray(tool.getIntent).map(translateString),
        inputParams,
        outputParams,
        translateOptionalArray(tool.getBaseCommand).map(translateString),
        arguments,
        stdin,
        stdout,
        stderr,
        requirements,
        Requirement.applyHints(tool.getHints, allSchemaDefs, ctx.hintSchemas),
        translateOptionalArray(tool.getSuccessCodes) match {
          case v if v.isEmpty => Set(0)
          case v              => v.map(translateInt).toSet
        },
        translateOptionalArray(tool.getTemporaryFailCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getPermanentFailCodes).map(translateInt).toSet
    )
  }
}
