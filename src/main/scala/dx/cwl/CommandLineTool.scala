package dx.cwl

import java.nio.file.Path

import dx.cwl.Utils.{
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

object StdFile extends Enumeration {
  type StdFile = Value
  val Stdin, Stdout, Stderr = Value
}

case class SecondaryFile(pattern: CwlValue, required: CwlValue)

object SecondaryFile {
  def apply(secondaryFile: SecondaryFileSchemaImpl,
            schemaDefs: Map[String, CwlSchema]): SecondaryFile = {
    SecondaryFile(CwlValue(secondaryFile.getPattern, schemaDefs),
                  CwlValue(secondaryFile.getRequired, schemaDefs))
  }
}

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

case class CommandInputParameter(id: Option[String],
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

object CommandInputParameter {
  def apply(
      param: CommandInputParameterImpl,
      schemaDefs: Map[String, CwlSchema]
  ): (CommandInputParameter, Boolean) = {
    val (types, stdfile) = CwlType(param.getType, schemaDefs)
    val inparam = CommandInputParameter(
        translateOptional(param.getId),
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

case class CommandOutputParameter(id: Option[String],
                                  label: Option[String],
                                  doc: Option[String],
                                  types: Vector[CwlType],
                                  outputBinding: Option[CommandOutputBinding],
                                  secondaryFiles: Vector[SecondaryFile],
                                  format: Vector[CwlValue],
                                  streamable: Option[Boolean])

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
        translateOptional(param.getId),
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

case class CommandLineTool(source: Option[String],
                           cwlVersion: Option[CWLVersion],
                           id: Option[String],
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
                           hints: Vector[Requirement],
                           successCodes: Set[Int],
                           temporaryFailCodes: Set[Int],
                           permanentFailCodes: Set[Int])
    extends DocumentElement

object CommandLineTool {
  def apply(tool: CommandLineToolImpl,
            source: Option[String] = None,
            schemaDefs: Map[String, CwlSchema] = Map.empty): CommandLineTool = {
    val (requirements, allSchemaDefs) =
      translateOptionalArray(tool.getRequirements)
        .foldLeft(Vector.empty[Requirement], schemaDefs) {
          case ((reqAccu, defAccu), req: ProcessRequirement) =>
            val cwlRequirement = Requirement(req, defAccu)
            cwlRequirement match {
              case SchemaDefRequirement(typeDefs) =>
                (reqAccu, defAccu ++ typeDefs.map(d => d.name.get -> d))
              case _ =>
                (reqAccu, defAccu)
            }
          case (_, req) =>
            throw new RuntimeException(s"unexpected requirement value ${req}")
        }

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
        CwlValue(s"$${inputs.${param.id}.path}", allSchemaDefs)
      }
      .orElse(toolStdin)

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
      case expr => ExprArgument(CwlValue(expr, allSchemaDefs))
    }

    CommandLineTool(
        source,
        translateOptional(tool.getCwlVersion),
        translateOptional(tool.getId),
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
        //        translateOptionalArray(tool.getHints).map {
        //          case req: ProcessRequirement => Requirement(req)
        //          case other =>
        //            throw new RuntimeException(s"unexpected hint value ${other}")
        //        },
        // TODO: for now, hints is left empty because cwljava doesn't parse them
        Vector.empty,
        translateOptionalArray(tool.getSuccessCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getTemporaryFailCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getPermanentFailCodes).map(translateInt).toSet
    )
  }

  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None): CommandLineTool = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull) match {
      case tool: CommandLineToolImpl => CommandLineTool(tool, Some(path.toString))
      case other =>
        throw new RuntimeException(s"Expected CommandLineTool, found ${other}")
    }
  }
}
