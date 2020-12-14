package dx.cwl

import java.nio.file.Path
import dx.cwl.Utils.{
  toStringAnyMap,
  translateDoc,
  translateInt,
  translateOptional,
  translateOptionalArray,
  translateOptionalObject,
  translateString
}
import org.w3id.cwl.cwl1_2.{
  CWLVersion,
  CommandInputParameterImpl,
  CommandLineBindingImpl,
  CommandLineToolImpl,
  CommandOutputBindingImpl,
  CommandOutputParameterImpl,
  LoadListingEnum,
  ProcessRequirement,
  SecondaryFileSchemaImpl
}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

import scala.jdk.CollectionConverters._

// https://www.commonwl.org/v1.2/CommandLineTool.html#LoadListingEnum
object LoadListing extends Enumeration {
  type LoadListing = Value
  val No, Shallow, Deep = Value

  def from(loadListing: LoadListingEnum): LoadListing = {
    loadListing match {
      case LoadListingEnum.NO_LISTING      => No
      case LoadListingEnum.SHALLOW_LISTING => Shallow
      case LoadListingEnum.DEEP_LISTING    => Deep
    }
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#stdin
// https://www.commonwl.org/v1.2/CommandLineTool.html#stdout
// https://www.commonwl.org/v1.2/CommandLineTool.html#stderr
object StdFile extends Enumeration {
  type StdFile = Value
  val Stdin, Stdout, Stderr = Value
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#SecondaryFileSchema
case class SecondaryFile(pattern: CwlValue, required: CwlValue)

object SecondaryFile {
  def apply(secondaryFile: SecondaryFileSchemaImpl,
            schemaDefs: Map[String, CwlSchema]): SecondaryFile = {
    SecondaryFile(CwlValue(secondaryFile.getPattern, schemaDefs),
                  CwlValue(secondaryFile.getRequired, schemaDefs))
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandLineBinding
case class CommandInputBinding(position: Option[CwlValue],
                               prefix: Option[String],
                               separate: Option[Boolean],
                               itemSeparator: Option[String],
                               shellQuote: Option[Boolean],
                               valueFrom: Option[CwlValue])

object CommandInputBinding {
  def apply(binding: CommandLineBindingImpl,
            schemaDefs: Map[String, CwlSchema]): CommandInputBinding = {
    CommandInputBinding(
        translateOptionalObject(binding.getPosition).map(CwlValue(_, schemaDefs)),
        translateOptional(binding.getPrefix),
        translateOptional(binding.getSeparate).map(_.booleanValue()),
        translateOptional(binding.getItemSeparator),
        translateOptional(binding.getShellQuote).map(_.booleanValue()),
        translateOptionalObject(binding.getValueFrom).map(CwlValue(_, schemaDefs))
    )
  }
}

sealed trait CommandParameter {
  val id: Option[Identifier]
  val label: Option[String]
  val doc: Option[String]
  val types: Vector[CwlType]
  val secondaryFiles: Vector[SecondaryFile]
  val format: Vector[CwlValue]
  val streamable: Option[Boolean]
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandInputParameter
case class CommandInputParameter(id: Option[Identifier],
                                 label: Option[String],
                                 doc: Option[String],
                                 types: Vector[CwlType],
                                 default: Option[CwlValue],
                                 inputBinding: Option[CommandInputBinding],
                                 secondaryFiles: Vector[SecondaryFile],
                                 format: Vector[CwlValue],
                                 streamable: Option[Boolean],
                                 loadContents: Option[Boolean],
                                 loadListing: Option[LoadListing.LoadListing])
    extends CommandParameter

object CommandInputParameter {
  def apply(
      param: CommandInputParameterImpl,
      schemaDefs: Map[String, CwlSchema]
  ): (CommandInputParameter, Boolean) = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    val inparam = CommandInputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getDefault).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        },
        translateOptionalArray(param.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        translateOptional(param.getLoadContents).map(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from)
    )
    (inparam, stdfile.contains(StdFile.Stdin))
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandOutputBinding
case class CommandOutputBinding(glob: Vector[CwlValue],
                                outputEval: Option[CwlValue],
                                loadContents: Option[Boolean],
                                loadListing: Option[LoadListing.LoadListing])

object CommandOutputBinding {
  def apply(binding: CommandOutputBindingImpl,
            schemaDefs: Map[String, CwlSchema]): CommandOutputBinding = {
    CommandOutputBinding(
        translateOptionalArray(binding.getGlob).map(CwlValue(_, schemaDefs)),
        translateOptional(binding.getOutputEval).map(CwlValue(_, schemaDefs)),
        translateOptional(binding.getLoadContents).map(_.booleanValue()),
        translateOptional(binding.getLoadListing).map(LoadListing.from)
    )
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandOutputParameter
case class CommandOutputParameter(id: Option[Identifier],
                                  label: Option[String],
                                  doc: Option[String],
                                  types: Vector[CwlType],
                                  outputBinding: Option[CommandOutputBinding],
                                  secondaryFiles: Vector[SecondaryFile],
                                  format: Vector[CwlValue],
                                  streamable: Option[Boolean])
    extends CommandParameter

object CommandOutputParameter {
  def apply(
      param: CommandOutputParameterImpl,
      schemaDefs: Map[String, CwlSchema]
  ): (CommandOutputParameter, Option[StdFile.StdFile]) = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    val outputBinding = translateOptional(param.getOutputBinding) match {
      case Some(_) if stdfile.nonEmpty =>
        throw new RuntimeException(s"outputBinding not allowed for type ${stdfile.get}")
      case Some(binding: CommandOutputBindingImpl) =>
        Some(CommandOutputBinding(binding, schemaDefs))
      case None if stdfile.nonEmpty =>
        Some(CommandOutputBinding(Vector(RandomFile(stdfile.get)), None, None, None))
      case None =>
        None
      case other =>
        throw new RuntimeException(s"unexpected CommandOutputBinding value ${other}")
    }
    val streamable = if (stdfile.nonEmpty) {
      Some(true)
    } else {
      translateOptional(param.getStreamable).map(_.booleanValue())
    }
    val outparam = CommandOutputParameter(
        translateOptional(param.getId).map(Identifier.apply),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        outputBinding,
        translateOptionalArray(param.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(param.getFormat).map(CwlValue(_, schemaDefs)),
        streamable
    )
    (outparam, stdfile)
  }
}

sealed trait Argument
case class ExprArgument(expr: CwlValue) extends Argument
case class BindingArgument(binding: CommandInputBinding) extends Argument

// https://www.commonwl.org/v1.2/CommandLineTool.html#CommandOutputBinding
case class CommandLineTool(source: Option[String],
                           cwlVersion: Option[CWLVersion],
                           id: Identifier,
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
    extends Process

object CommandLineTool {

  /**
    * Creates a [[CommandLineTool]] from the [[CommandLineToolImpl]] created by the Java parser.
    * @param tool the Java object
    * @param source the CWL original source code
    * @param schemaDefs any schema definitions to use when resolving types
    * @return a [[CommandLineTool]]
    */
  def apply(tool: CommandLineToolImpl,
            source: Option[Path] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty,
            hintSchemas: Map[String, HintSchema] = Map.empty): CommandLineTool = {
    val (requirements, allSchemaDefs) =
      translateOptionalArray(tool.getRequirements)
        .foldLeft(Vector.empty[Requirement], schemaDefs) {
          case ((reqAccu, defAccu), req: ProcessRequirement) =>
            val cwlRequirement = Requirement(req, defAccu)
            val newSchemaDefs = cwlRequirement match {
              // add any new schema defs to the initial set
              case SchemaDefRequirement(typeDefs) =>
                defAccu ++ typeDefs.map(d => d.name.get -> d)
              case _ =>
                defAccu
            }
            (reqAccu :+ cwlRequirement, newSchemaDefs)
          case (_, req) =>
            throw new RuntimeException(s"unexpected requirement value ${req}")
        }

    val hints = translateOptionalArray(tool.getHints).map {
      case attrs: java.util.Map[_, _] =>
        val rawHints = attrs.asScala.toMap match {
          case hints: Map[_, _] => toStringAnyMap(hints)
          case other            => throw new Exception(s"invalid hints ${other.getClass}")
        }
        Requirement.apply(rawHints, allSchemaDefs, hintSchemas)
      case other =>
        throw new Exception(s"unexpected hints value ${other}")
    }

    // An input may have type `stdin`, which is a file that is created from the
    // standard input piped to the CommandLineTool. A maximum of one input parameter
    // can have the `stdin` type, and if there is one then the tool's `stdin`
    // attribute cannot be set.
    // https://www.commonwl.org/v1.2/CommandLineTool.html#stdin
    val (inputParams, isStdin) = tool.getInputs.asScala
      .map {
        case param: CommandInputParameterImpl => CommandInputParameter(param, allSchemaDefs)
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
          .flatMap(_.name)
          .getOrElse(
              throw new Exception(s"missing input parameter ${paramStdin}")
          )
        CwlValue(s"$${inputs.${paramId}.path}", allSchemaDefs)
      }
      .orElse(toolStdin)

    // An output parameter may have type `stdout` or `stderr`. There can be a maximum of
    // one output parameter with each type, and if there is one then the corresponding
    // CommandLineTool attribute cannot also be set.
    // https://www.commonwl.org/v1.2/CommandLineTool.html#stdout
    val (outputParams, stdfile) = tool.getOutputs.asScala
      .map {
        case param: CommandOutputParameterImpl => CommandOutputParameter(param, allSchemaDefs)
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
        BindingArgument(CommandInputBinding(binding, allSchemaDefs))
      case expr =>
        ExprArgument(CwlValue(expr, allSchemaDefs))
    }

    val id = (translateOptional(tool.getId), source) match {
      case (Some(id), _) => Identifier(id)
      case (None, Some(path)) if path.endsWith(".cwl") =>
        Identifier(namespace = None, name = Some(path.getFileName.toString.dropRight(4)))
      case _ =>
        throw new Exception("either tool id or file path must be defined")
    }

    CommandLineTool(
        source.map(_.toString),
        translateOptional(tool.getCwlVersion),
        id,
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
        hints,
        translateOptionalArray(tool.getSuccessCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getTemporaryFailCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getPermanentFailCodes).map(translateInt).toSet
    )
  }

  /**
    * Parses a CommandLineTool CWL document.
    * @param path path to the document
    * @param baseUri base URI to use when loading imports
    * @param loadingOptions document loading options
    * @return a [[CommandLineTool]]
    */
  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty,
            hintSchemas: Map[String, HintSchema] = Map.empty): CommandLineTool = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull) match {
      case tool: CommandLineToolImpl => CommandLineTool(tool, Some(path), schemaDefs, hintSchemas)
      case other =>
        throw new RuntimeException(s"Expected CommandLineTool, found ${other}")
    }
  }
}
