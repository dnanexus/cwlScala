package cwl

import java.nio.file.Path

import com.sun.jdi.connect.Connector.StringArgument
import org.w3id.cwl.cwl1_2.{
  CWLType,
  CWLVersion,
  CommandInputArraySchemaImpl,
  CommandInputEnumSchemaImpl,
  CommandInputParameterImpl,
  CommandInputRecordFieldImpl,
  CommandInputRecordSchemaImpl,
  CommandLineBindingImpl,
  CommandLineToolImpl,
  CommandOutputBindingImpl,
  CommandOutputParameterImpl,
  LoadListingEnum,
  ProcessRequirement,
  SecondaryFileSchemaImpl,
  WorkflowImpl,
  stdin => CWLStdin
}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

sealed trait CwlType

object CwlType {
  case object T_String extends CwlType
  case object T_Boolean extends CwlType
  case object T_Int extends CwlType
  case object T_Long extends CwlType
  case object T_Float extends CwlType
  case object T_Double extends CwlType
  case object T_File extends CwlType
  case object T_Directory extends CwlType
  case object T_Null extends CwlType

  sealed trait CwlSchemaType extends CwlType {
    val name: Option[String]
    val label: Option[String]
    val doc: Option[String]
    val inputBinding: Option[CommandInputBinding]
  }

  case class T_Array(itemTypes: Vector[CwlType],
                     name: Option[String],
                     label: Option[String],
                     doc: Option[String],
                     inputBinding: Option[CommandInputBinding])
      extends CwlSchemaType

  case class RecordField(name: String,
                         label: Option[String],
                         doc: Option[String],
                         types: Vector[CwlType],
                         inputBinding: Option[CommandInputBinding],
                         secondaryFiles: Vector[SecondaryFile],
                         format: Vector[CwlExpr],
                         streamable: Option[Boolean],
                         loadContents: Option[Boolean],
                         loadListing: Option[LoadListingEnum])

  case class T_Record(fields: Vector[RecordField],
                      name: Option[String],
                      label: Option[String],
                      doc: Option[String],
                      inputBinding: Option[CommandInputBinding])
      extends CwlSchemaType

  case class T_Enum(symbols: Vector[String],
                    name: Option[String],
                    label: Option[String],
                    doc: Option[String],
                    inputBinding: Option[CommandInputBinding])
      extends CwlSchemaType
}

sealed trait CwlExpr

case class SecondaryFile(pattern: CwlExpr, required: CwlExpr)

case class CommandInputBinding(position: Option[CwlExpr],
                               prefix: Option[String],
                               separate: Option[Boolean],
                               itemSeparator: Option[String],
                               shellQuote: Option[Boolean],
                               valueFrom: Option[CwlExpr])

case class CommandInputParameter(id: Option[String],
                                 label: Option[String],
                                 doc: Option[String],
                                 types: Vector[CwlType],
                                 default: Option[CwlExpr],
                                 inputBinding: Option[CommandInputBinding],
                                 secondaryFiles: Vector[SecondaryFile],
                                 format: Vector[CwlExpr],
                                 streamable: Option[Boolean],
                                 loadContents: Option[Boolean],
                                 loadListing: Option[LoadListingEnum])

case class CommandOutputBinding(glob: Vector[CwlExpr],
                                outputEval: Option[CwlExpr],
                                loadContents: Option[Boolean],
                                loadListing: Option[LoadListingEnum])

case class CommandOutputParameter(id: Option[String],
                                  label: Option[String],
                                  doc: Option[String],
                                  types: Vector[CwlType],
                                  outputBinding: Option[CommandOutputBinding],
                                  secondaryFiles: Vector[SecondaryFile],
                                  format: Vector[CwlExpr],
                                  streamable: Option[Boolean])

sealed trait Argument
case class ExprArgument(expr: CwlExpr) extends Argument
case class BindingArgument(binding: CommandInputBinding) extends Argument

case class CommandLineTool(cwlVersion: Option[CWLVersion],
                           id: Option[String],
                           label: Option[String],
                           doc: Option[String],
                           intent: Vector[String],
                           inputs: Vector[CommandInputParameter],
                           outputs: Vector[CommandOutputParameter],
                           baseCommand: Vector[String],
                           arguments: Vector[Argument],
                           stdin: Option[CwlExpr],
                           stdout: Option[CwlExpr],
                           stderr: Option[CwlExpr],
                           requirements: Any,
                           hints: Any,
                           successCodes: Set[Int],
                           temporaryFailCodes: Set[Int],
                           permanentFailCodes: Set[Int])

case class Workflow()

object Parser {
  private def translateOptional[T](opt: java.util.Optional[T]): Option[T] = {
    opt match {
      case null => None
      case _    => opt.toScala
    }
  }

  private def translateOptionalObject(obj: java.lang.Object): Option[java.lang.Object] = {
    obj match {
      case null                                      => None
      case opt: java.util.Optional[java.lang.Object] => translateOptional(opt)
      case _                                         => Some(obj)
    }
  }

  private def translateOptionalArray(obj: java.lang.Object): Vector[java.lang.Object] = {
    obj match {
      case null => Vector.empty
      case opt: java.util.Optional[java.lang.Object] =>
        translateOptional(opt) match {
          case array: java.util.List[java.lang.Object] => array.asScala.toVector
          case value                                   => Vector(value)
        }
      case array: java.util.List[java.lang.Object] => array.asScala.toVector
      case _                                       => Vector(obj)
    }
  }

  private def translateString(obj: java.lang.Object): String = {
    obj match {
      case s: String => s
      case _         => throw new RuntimeException(s"unexpected string value ${obj}")
    }
  }

  private def translateInt(obj: java.lang.Object): Int = {
    obj match {
      case i: java.lang.Integer => i.toInt
      case _ =>
        throw new RuntimeException(s"unexpected int value ${obj}")
    }
  }

  private def translateDoc(obj: java.lang.Object): Option[String] = {
    obj match {
      case null                 => None
      case s: String            => Some(s)
      case a: java.util.List[_] => Some(a.asScala.mkString("\n"))
      case _ =>
        throw new RuntimeException(s"unexpected doc value ${obj}")
    }
  }

  private def translateExpr(expr: java.lang.Object): CwlExpr = {}

  private def translateCommandLineBinding(obj: Any): CommandInputBinding = {
    obj match {
      case binding: CommandLineBindingImpl =>
        CommandInputBinding(
            translateOptionalObject(binding.getPosition).map(translateExpr),
            translateOptional(binding.getPrefix),
            translateOptional(binding.getSeparate).map(_.booleanValue()),
            translateOptional(binding.getItemSeparator),
            translateOptional(binding.getShellQuote).map(_.booleanValue()),
            translateOptionalObject(binding.getValueFrom).map(translateExpr)
        )
      case _ =>
        throw new RuntimeException(s"unexpected CommandLineBinding value ${obj}")
    }
  }

  private def translateArraySchema(schema: CommandInputArraySchemaImpl): CwlType.T_Array = {
    val (types, isStdin) = translateType(schema.getItems)
    assert(!isStdin)
    CwlType.T_Array(
        types,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map(translateCommandLineBinding)
    )
  }

  private def translateSecondaryFile(obj: java.lang.Object): SecondaryFile = {
    obj match {
      case sf: SecondaryFileSchemaImpl =>
        SecondaryFile(translateExpr(sf.getPattern), translateExpr(sf.getRequired))
      case _ =>
        throw new RuntimeException(s"unexpected SecondaryFile value ${obj}")
    }
  }

  private def translateRecordField(field: java.lang.Object): CwlType.RecordField = {
    field match {
      case f: CommandInputRecordFieldImpl =>
        val (types, isStdin) = translateType(f.getType)
        assert(!isStdin)
        CwlType.RecordField(
            f.getName,
            translateOptional(f.getLabel),
            translateDoc(f.getDoc),
            types,
            translateOptional(f.getInputBinding).map(translateCommandLineBinding),
            translateOptionalArray(f.getSecondaryFiles).map(translateSecondaryFile),
            translateOptionalArray(f.getFormat).map(translateExpr),
            translateOptional(f.getStreamable).map(_.booleanValue()),
            translateOptional(f.getLoadContents).map(_.booleanValue()),
            translateOptional(f.getLoadListing)
        )
      case _ =>
        throw new RuntimeException(s"invalid record field ${field}")
    }
  }

  private def translateRecordSchema(schema: CommandInputRecordSchemaImpl): CwlType.T_Record = {
    CwlType.T_Record(
        translateOptional(schema.getFields)
          .map(_.asScala.map(translateRecordField).toVector)
          .getOrElse(Vector.empty),
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map(translateCommandLineBinding)
    )
  }
  private def translateEnumSchema(schema: CommandInputEnumSchemaImpl): CwlType.T_Enum = {
    CwlType.T_Enum(
        schema.getSymbols.asScala.map {
          case s: String => s
          case other     => throw new Exception(s"unexpected symbol value ${other}")
        }.toVector,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map(translateCommandLineBinding)
    )
  }

  private def translateType(t: java.lang.Object): (Vector[CwlType], Boolean) = {
    t match {
      case a: java.util.List[java.lang.Object] =>
        val (cwlTypes, isStdin) = a.asScala.map(translateType).unzip
        (cwlTypes.flatten.toVector, isStdin.contains(true))
      case CWLStdin.STDIN => (Vector(CwlType.T_File), true)
      case _ =>
        val cwlType: CwlType = t match {
          case "string"                             => CwlType.T_String
          case "boolean"                            => CwlType.T_Boolean
          case "int"                                => CwlType.T_Int
          case "long"                               => CwlType.T_Long
          case "float"                              => CwlType.T_Float
          case "double"                             => CwlType.T_Double
          case "null"                               => CwlType.T_Null
          case CWLType.FILE                         => CwlType.T_File
          case CWLType.DIRECTORY                    => CwlType.T_Directory
          case schema: CommandInputArraySchemaImpl  => translateArraySchema(schema)
          case schema: CommandInputRecordSchemaImpl => translateRecordSchema(schema)
          case schema: CommandInputEnumSchemaImpl   => translateEnumSchema(schema)
        }
        (Vector(cwlType), false)
    }
  }

  private def translateCommandInputParameter(
      param: CommandInputParameterImpl
  ): (CommandInputParameter, Boolean) = {
    val (types, isStdin) = translateType(param.getType)
    val cwlParam = CommandInputParameter(
        translateOptional(param.getId),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getDefault).map(translateExpr),
        translateOptional(param.getInputBinding).map(translateCommandLineBinding),
        translateOptionalArray(param.getSecondaryFiles).map(translateSecondaryFile),
        translateOptionalArray(param.getFormat).map(translateExpr),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        translateOptional(param.getLoadContents).map(_.booleanValue()),
        translateOptional(param.getLoadListing)
    )
    (cwlParam, isStdin)
  }

  private def translateOutputBinding(binding: CommandOutputBindingImpl): CommandOutputBinding = {
    CommandOutputBinding(
        translateOptionalArray(binding.getGlob).map(translateExpr),
        translateOptional(binding.getOutputEval).map(translateExpr),
        translateOptional(binding.getLoadContents).map(_.booleanValue()),
        translateOptional(binding.getLoadListing)
    )
  }

  private def translateCommandOutputParameter(
      param: CommandOutputParameterImpl
  ): CommandOutputParameter = {
    val (types, isStdin) = translateType(param.getType)
    assert(!isStdin)
    CommandOutputParameter(
        translateOptional(param.getId),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getOutputBinding).map {
          case binding: CommandOutputBindingImpl => translateOutputBinding(binding)
          case other =>
            throw new RuntimeException(s"unexpected CommandOutputBinding value ${other}")
        },
        translateOptionalArray(param.getSecondaryFiles).map(translateSecondaryFile),
        translateOptionalArray(param.getFormat).map(translateExpr),
        translateOptional(param.getStreamable).map(_.booleanValue())
    )
  }

  private def translateRequirement(requirement: ProcessRequirement): Any = {}

  def translateTool(tool: CommandLineToolImpl): CommandLineTool = {
    val (inputParams, isStdin) = tool.getInputs.asScala
      .map {
        case param: CommandInputParameterImpl => translateCommandInputParameter(param)
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
    val toolStdin: Option[CwlExpr] = translateOptionalObject(tool.getStdin).map(translateExpr)
    if (paramStdin.isDefined && toolStdin.isDefined) {
      throw new RuntimeException(
          s"'stdin' specified at both the tool level and by parameter ${paramStdin.get}"
      )
    }
    val stdin = paramStdin
      .map { param =>
        translateExpr(s"$${inputs.${param.id}.path}")
      }
      .orElse(toolStdin)
    val outputParams = tool.getOutputs.asScala.map {
      case param: CommandOutputParameterImpl => translateCommandOutputParameter(param)
      case other =>
        throw new RuntimeException(s"unexpected CommandOutputParameter value ${other}")
    }.toVector
    val arguments = translateOptionalArray(tool.getArguments).map {
      case binding: CommandLineBindingImpl => BindingArgument(translateCommandLineBinding(binding))
      case expr                            => ExprArgument(translateExpr(expr))
    }
    CommandLineTool(
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
        translateOptionalObject(tool.getStdout).map(translateExpr),
        translateOptionalObject(tool.getStderr).map(translateExpr),
        translateOptionalArray(tool.getRequirements).map {
          case req: ProcessRequirement => translateRequirement(req)
          case other =>
            throw new RuntimeException(s"unexpected requirement value ${other}")
        },
        translateOptionalArray(tool.getHints).map(translateExpr),
        translateOptionalArray(tool.getSuccessCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getTemporaryFailCodes).map(translateInt).toSet,
        translateOptionalArray(tool.getPermanentFailCodes).map(translateInt).toSet
    )
  }

  def translateWorkflow(workflow: WorkflowImpl): Workflow = { null }

  /**
    * Parse a CWL document.
    * @param path path to the CWL document.
    * @param baseUri base path to make paths absolute - defaults to cwd
    * @param loadingOptions options to use for document loading
    * @return one of the top-level CWL object types (CommandLineTool, ExpressionTool, Workflow, Operation, or an
    *         array of these)
    */
  def parse(path: Path,
            baseUri: Option[String] = None,
            loadingOptions: Option[LoadingOptions] = None): Any = {
    RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull)
  }

  def parseTool(path: Path,
                baseUri: Option[String] = None,
                loadingOptions: Option[LoadingOptions] = None): CommandLineTool = {
    parse(path, baseUri, loadingOptions) match {
      case tool: CommandLineToolImpl => translateTool(tool)
      case other =>
        throw new RuntimeException(s"Expected CommandLineTool, found ${other}")
    }
  }

  def parseWorkflow(path: Path,
                    baseUri: Option[String] = None,
                    loadingOptions: Option[LoadingOptions] = None): Workflow = {
    parse(path, baseUri, loadingOptions) match {
      case workflow: WorkflowImpl => translateWorkflow(workflow)
      case other =>
        throw new RuntimeException(s"Expected Workflow, found ${other}")
    }
  }
}
