package cwl

import java.nio.file.Path

import cwl.ParserUtils._
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
  CommandOutputArraySchemaImpl,
  CommandOutputBindingImpl,
  CommandOutputEnumSchemaImpl,
  CommandOutputParameterImpl,
  CommandOutputRecordFieldImpl,
  CommandOutputRecordSchemaImpl,
  DirectoryImpl,
  DockerRequirementImpl,
  EnvVarRequirementImpl,
  EnvironmentDefImpl,
  FileImpl,
  InitialWorkDirRequirementImpl,
  InlineJavascriptRequirementImpl,
  InplaceUpdateRequirementImpl,
  LoadListingEnum,
  LoadListingRequirementImpl,
  NetworkAccessImpl,
  ProcessRequirement,
  ResourceRequirementImpl,
  SchemaDefRequirementImpl,
  SecondaryFileSchemaImpl,
  ShellCommandRequirementImpl,
  SoftwarePackageImpl,
  SoftwareRequirementImpl,
  ToolTimeLimitImpl,
  WorkReuseImpl,
  WorkflowImpl,
  stdin => CWLStdin,
  stdout => CWLStdout,
  stderr => CWLStderr
}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

private object ParserUtils {
  implicit class IterableExtensions[A](iterable: Iterable[A]) {
    def asOption: Option[IterableOnce[A]] = {
      if (iterable.nonEmpty) {
        Some(iterable)
      } else {
        None
      }
    }
  }

  def translateOptional[T](opt: java.util.Optional[T]): Option[T] = {
    opt match {
      case null => None
      case _    => opt.toScala
    }
  }

  def translateOptionalObject(obj: java.lang.Object): Option[java.lang.Object] = {
    obj match {
      case null => None
      case opt: java.util.Optional[_] =>
        translateOptional(opt.asInstanceOf[java.util.Optional[java.lang.Object]])
      case _ => Some(obj)
    }
  }

  def translateArray(obj: java.lang.Object): Vector[java.lang.Object] = {
    obj match {
      case array: java.util.List[_] =>
        array.asInstanceOf[java.util.List[java.lang.Object]].asScala.toVector
      case _ => Vector(obj)
    }
  }

  def translateOptionalArray(obj: java.lang.Object): Vector[java.lang.Object] = {
    obj match {
      case null => Vector.empty
      case opt: java.util.Optional[_] =>
        translateOptional(opt) match {
          case Some(array: java.util.List[_]) =>
            array.asInstanceOf[java.util.List[java.lang.Object]].asScala.toVector
          case Some(value) => Vector(value.asInstanceOf[java.lang.Object])
          case None        => Vector.empty
        }
      case _ => translateArray(obj)
    }
  }

  def translateString(obj: java.lang.Object): String = {
    obj match {
      case s: String => s
      case _         => throw new RuntimeException(s"unexpected string value ${obj}")
    }
  }

  def translateInt(obj: java.lang.Object): Int = {
    obj match {
      case i: java.lang.Integer => i.toInt
      case _ =>
        throw new RuntimeException(s"unexpected int value ${obj}")
    }
  }

  def translateDoc(obj: java.lang.Object): Option[String] = {
    obj match {
      case null                 => None
      case s: String            => Some(s)
      case a: java.util.List[_] => Some(a.asScala.mkString("\n"))
      case _ =>
        throw new RuntimeException(s"unexpected doc value ${obj}")
    }
  }
}

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
                         loadListing: Option[LoadListing.LoadListing])

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

  def apply(schema: CommandInputArraySchemaImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType]): T_Array = {
    val (types, stdfile) = apply(schema.getItems, typeAliases)
    assert(stdfile.isEmpty)
    T_Array(
        types,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
    )
  }

  def apply(field: CommandInputRecordFieldImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType]): RecordField = {
    val (types, stdfile) = apply(field.getType, typeAliases)
    assert(stdfile.isEmpty)
    RecordField(
        field.getName,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
        types,
        translateOptional(field.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        },
        translateOptionalArray(field.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(field.getFormat).map(CwlExpr.apply),
        translateOptional(field.getStreamable).map(_.booleanValue()),
        translateOptional(field.getLoadContents).map(_.booleanValue()),
        translateOptional(field.getLoadListing).map(LoadListing.from)
    )
  }

  def apply(schema: CommandInputRecordSchemaImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType]): CwlType.T_Record = {
    CwlType.T_Record(
        translateOptional(schema.getFields)
          .map(_.asScala.map {
            case field: CommandInputRecordFieldImpl => apply(field, typeAliases)
            case other =>
              throw new RuntimeException(s"invalid record field ${other}")
          }.toVector)
          .getOrElse(Vector.empty),
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
    )
  }

  def apply(schema: CommandInputEnumSchemaImpl): CwlType.T_Enum = {
    CwlType.T_Enum(
        schema.getSymbols.asScala.map {
          case s: String => s
          case other     => throw new Exception(s"unexpected symbol value ${other}")
        }.toVector,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
    )
  }

  def apply(schema: CommandOutputArraySchemaImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType]): T_Array = {
    val (types, stdfile) = apply(schema.getItems, typeAliases)
    assert(stdfile.isEmpty)
    T_Array(
        types,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        None
    )
  }

  def apply(field: CommandOutputRecordFieldImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType]): RecordField = {
    val (types, stdfile) = apply(field.getType, typeAliases)
    assert(stdfile.isEmpty)
    RecordField(
        field.getName,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
        types,
        None,
        translateOptionalArray(field.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(field.getFormat).map(CwlExpr.apply),
        translateOptional(field.getStreamable).map(_.booleanValue()),
        None,
        None
    )
  }

  def apply(schema: CommandOutputRecordSchemaImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType] = Map.empty): CwlType.T_Record = {
    CwlType.T_Record(
        translateOptional(schema.getFields)
          .map(_.asScala.map {
            case field: CommandOutputRecordFieldImpl => apply(field, typeAliases)
            case other =>
              throw new RuntimeException(s"invalid record field ${other}")
          }.toVector)
          .getOrElse(Vector.empty),
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        None
    )
  }

  def apply(schema: CommandOutputEnumSchemaImpl): CwlType.T_Enum = {
    CwlType.T_Enum(
        schema.getSymbols.asScala.map {
          case s: String => s
          case other     => throw new Exception(s"unexpected symbol value ${other}")
        }.toVector,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        None
    )
  }

  def apply(
      t: java.lang.Object,
      typeAliases: Map[String, CwlType.CwlSchemaType]
  ): (Vector[CwlType], Option[StdFile.StdFile]) = {
    t match {
      case a: java.util.List[_] =>
        val (cwlTypes, stdfile) =
          a.asInstanceOf[java.util.List[java.lang.Object]].asScala.map(apply(_, typeAliases)).unzip
        (cwlTypes.flatten.toVector, stdfile.flatten.toSet.headOption)
      case CWLStdin.STDIN   => (Vector(CwlType.T_File), Some(StdFile.Stdin))
      case CWLStdout.STDOUT => (Vector(CwlType.T_File), Some(StdFile.Stdout))
      case CWLStderr.STDERR => (Vector(CwlType.T_File), Some(StdFile.Stderr))
      case _ =>
        val cwlType: CwlType = t match {
          case "string"                              => CwlType.T_String
          case "boolean"                             => CwlType.T_Boolean
          case "int"                                 => CwlType.T_Int
          case "long"                                => CwlType.T_Long
          case "float"                               => CwlType.T_Float
          case "double"                              => CwlType.T_Double
          case "null"                                => CwlType.T_Null
          case CWLType.FILE                          => CwlType.T_File
          case CWLType.DIRECTORY                     => CwlType.T_Directory
          case schema: CommandInputArraySchemaImpl   => apply(schema, typeAliases)
          case schema: CommandInputRecordSchemaImpl  => apply(schema, typeAliases)
          case schema: CommandInputEnumSchemaImpl    => apply(schema)
          case schema: CommandOutputArraySchemaImpl  => apply(schema, typeAliases)
          case schema: CommandOutputRecordSchemaImpl => apply(schema, typeAliases)
          case schema: CommandOutputEnumSchemaImpl   => apply(schema)
          case s: String if s.contains("#")          =>
            // a schema reference
            val schemaName = s.drop(s.indexOf("#") + 1)
            typeAliases.get(schemaName) match {
              case Some(schemaDef) => schemaDef
              case None =>
                throw new RuntimeException(s"missing definition for schema ${schemaName}")
            }
          case other =>
            throw new RuntimeException(s"unexpected expression ${other}")
        }
        (Vector(cwlType), None)
    }
  }
}

sealed trait CwlExpr

object CwlExpr {
  case object NullValue extends CwlExpr
  case class StringValue(value: String) extends CwlExpr
  case class BooleanValue(value: Boolean) extends CwlExpr
  case class IntValue(value: Int) extends CwlExpr
  case class LongValue(value: Long) extends CwlExpr
  case class FloatValue(value: Float) extends CwlExpr
  case class DoubleValue(value: Double) extends CwlExpr
  sealed trait PathValue extends CwlExpr {
    val location: Option[String]
    val path: Option[String]
    val basename: Option[String]
  }
  case class FileValue(location: Option[String],
                       path: Option[String],
                       basename: Option[String],
                       dirname: Option[String],
                       nameroot: Option[String],
                       nameext: Option[String],
                       checksum: Option[String],
                       size: Option[Long],
                       secondaryFiles: Vector[PathValue],
                       format: Option[String],
                       contents: Option[String])
      extends PathValue
  case class DirectoryValue(location: Option[String],
                            path: Option[String],
                            basename: Option[String],
                            listing: Vector[PathValue])
      extends PathValue

  /**
    * Indicates that a random filename should be generated to store the
    * contents of stdout/stderr.
    */
  case class RandomFile(stdfile: StdFile.StdFile) extends CwlExpr

  /**
    * An expression that requires evaluation by a Javascript engine.
    * @param value the expression
    */
  case class JavascriptExpr(value: String) extends CwlExpr

  /**
    * A string that includes placeholders.
    * @param parts the parts of the string - alternating `StringValue` and `CwlExpr` objects.
    * @example
    * "my name is ${name}" -> CompoundString(StringValue("my name is "), JavascriptExpr("name"))
    */
  case class CompoundString(parts: Vector[CwlExpr]) extends CwlExpr

  private def translatePathValue(expr: java.lang.Object): PathValue = {
    expr match {
      case file: FileImpl           => apply(file)
      case directory: DirectoryImpl => apply(directory)
      case _ =>
        throw new RuntimeException(s"unexpected file/directory value ${expr}")
    }
  }

  def apply(file: FileImpl): FileValue = {
    FileValue(
        translateOptional(file.getLocation),
        translateOptional(file.getPath),
        translateOptional(file.getBasename),
        translateOptional(file.getDirname),
        translateOptional(file.getNameroot),
        translateOptional(file.getNameext),
        translateOptional(file.getChecksum),
        translateOptional(file.getSize).map(_.longValue()),
        translateOptionalArray(file.getSecondaryFiles).map(translatePathValue),
        translateOptional(file.getFormat),
        translateOptional(file.getContents)
    )
  }

  def apply(directory: DirectoryImpl): DirectoryValue = {
    DirectoryValue(
        translateOptional(directory.getLocation),
        translateOptional(directory.getPath),
        translateOptional(directory.getBasename),
        translateOptionalArray(directory.getListing).map(translatePathValue)
    )
  }

  private def translateString(obj: Any): Option[String] = {
    obj match {
      case null      => None
      case s: String => Some(s)
      case _         => throw new RuntimeException(s"unexpected string value ${obj}")
    }
  }

  private def translatePathMap(map: java.util.Map[_, _]): PathValue = {
    map.get("class") match {
      case null =>
        throw new RuntimeException(s"missing required key 'class' in expression ${map}")
      case "File"      => fileFromMap(map)
      case "Directory" => directoryFromMap(map)
      case other       => throw new RuntimeException(s"unsupported class ${other}")
    }
  }

  def fileFromMap(map: java.util.Map[_, _]): FileValue = {
    FileValue(
        translateString(map.get("location")),
        translateString(map.get("path")),
        translateString(map.get("basename")),
        translateString(map.get("dirname")),
        translateString(map.get("nameroot")),
        translateString(map.get("nameext")),
        translateString(map.get("checksum")),
        translateString(map.get("size")).map(_.toLong),
        map.get("secondaryFiles") match {
          case null => Vector.empty
          case pathList: java.util.List[_] =>
            pathList.asScala.map {
              case paths: java.util.Map[_, _] => translatePathMap(paths)
              case other =>
                throw new RuntimeException(s"unexpected path value ${other}")
            }.toVector
          case other =>
            throw new RuntimeException(s"unexpected secondaryFiles value ${other}")
        },
        translateString(map.get("format")),
        translateString(map.get("contents"))
    )
  }

  def directoryFromMap(map: java.util.Map[_, _]): DirectoryValue = {
    DirectoryValue(
        translateString(map.get("location")),
        translateString(map.get("path")),
        translateString(map.get("basename")),
        map.get("listing") match {
          case null => Vector.empty
          case pathList: java.util.List[_] =>
            pathList.asScala.map {
              case paths: java.util.Map[_, _] => translatePathMap(paths)
              case other =>
                throw new RuntimeException(s"unexpected path value ${other}")
            }.toVector
          case other =>
            throw new RuntimeException(s"unexpected secondaryFiles value ${other}")
        }
    )
  }

  def apply(expr: java.lang.Object): CwlExpr = {
    expr match {
      case m: java.util.Map[_, _] =>
        translatePathMap(m)
      case s: String =>
        val parts = s
          .split("((?=\\$\\{.+}))")
          .collect {
            case s if s.startsWith("${") && s.endsWith("}") =>
              CwlExpr.JavascriptExpr(s.substring(2, s.length - 1))
            case s => CwlExpr.StringValue(s)
          }
          .toVector
        if (parts.size > 1) {
          CompoundString(parts)
        } else {
          parts.head
        }
      case b: java.lang.Boolean =>
        BooleanValue(b.booleanValue())
      case i: java.lang.Integer =>
        IntValue(i.toInt)
      case l: java.lang.Long =>
        LongValue(l.toLong)
      case f: java.lang.Float =>
        FloatValue(f.toFloat)
      case d: java.lang.Double =>
        DoubleValue(d.toDouble)
      case bi: java.math.BigInteger =>
        try {
          LongValue(bi.longValueExact())
        } catch {
          case ex: ArithmeticException =>
            throw new RuntimeException(s"invalid long value ${bi.toString}", ex)
        }
      case bd: java.math.BigDecimal =>
        DoubleValue(bd.doubleValue())
      case file: FileImpl =>
        apply(file)
      case directory: DirectoryImpl =>
        apply(directory)
      case _ =>
        throw new RuntimeException(s"unexpected expression value ${expr}")
    }
  }
}

case class SecondaryFile(pattern: CwlExpr, required: CwlExpr)

object SecondaryFile {
  def apply(secondaryFile: SecondaryFileSchemaImpl): SecondaryFile = {
    SecondaryFile(CwlExpr(secondaryFile.getPattern), CwlExpr.apply(secondaryFile.getRequired))
  }
}

case class CommandInputBinding(position: Option[CwlExpr],
                               prefix: Option[String],
                               separate: Option[Boolean],
                               itemSeparator: Option[String],
                               shellQuote: Option[Boolean],
                               valueFrom: Option[CwlExpr])

object CommandInputBinding {
  def apply(binding: CommandLineBindingImpl): CommandInputBinding = {
    CommandInputBinding(
        translateOptionalObject(binding.getPosition).map(CwlExpr.apply),
        translateOptional(binding.getPrefix),
        translateOptional(binding.getSeparate).map(_.booleanValue()),
        translateOptional(binding.getItemSeparator),
        translateOptional(binding.getShellQuote).map(_.booleanValue()),
        translateOptionalObject(binding.getValueFrom).map(CwlExpr.apply)
    )
  }
}

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
                                 loadListing: Option[LoadListing.LoadListing])

object CommandInputParameter {
  def apply(
      param: CommandInputParameterImpl,
      typeAliases: Map[String, CwlType.CwlSchemaType]
  ): (CommandInputParameter, Boolean) = {
    val (types, stdfile) = CwlType(param.getType, typeAliases)
    val inparam = CommandInputParameter(
        translateOptional(param.getId),
        translateOptional(param.getLabel),
        translateDoc(param.getDoc),
        types,
        translateOptional(param.getDefault).map(CwlExpr.apply),
        translateOptional(param.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        },
        translateOptionalArray(param.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(param.getFormat).map(CwlExpr.apply),
        translateOptional(param.getStreamable).map(_.booleanValue()),
        translateOptional(param.getLoadContents).map(_.booleanValue()),
        translateOptional(param.getLoadListing).map(LoadListing.from)
    )
    (inparam, stdfile.contains(StdFile.Stdin))
  }
}

case class CommandOutputBinding(glob: Vector[CwlExpr],
                                outputEval: Option[CwlExpr],
                                loadContents: Option[Boolean],
                                loadListing: Option[LoadListing.LoadListing])

object CommandOutputBinding {
  def apply(binding: CommandOutputBindingImpl): CommandOutputBinding = {
    CommandOutputBinding(
        translateOptionalArray(binding.getGlob).map(CwlExpr.apply),
        translateOptional(binding.getOutputEval).map(CwlExpr.apply),
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
                                  format: Vector[CwlExpr],
                                  streamable: Option[Boolean])

object CommandOutputParameter {
  def apply(
      param: CommandOutputParameterImpl,
      typeAliases: Map[String, CwlType.CwlSchemaType]
  ): (CommandOutputParameter, Option[StdFile.StdFile]) = {
    val (types, stdfile) = CwlType(param.getType, typeAliases)
    val outputBinding = translateOptional(param.getOutputBinding) match {
      case Some(_) if stdfile.nonEmpty =>
        throw new RuntimeException(s"outputBinding not allowed for type ${stdfile.get}")
      case Some(binding: CommandOutputBindingImpl) =>
        Some(CommandOutputBinding(binding))
      case None if stdfile.nonEmpty =>
        Some(CommandOutputBinding(Vector(CwlExpr.RandomFile(stdfile.get)), None, None, None))
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
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(param.getFormat).map(CwlExpr.apply),
        streamable
    )
    (outparam, stdfile)
  }
}

sealed trait Requirement

case class InlineJavascriptRequirement(expressionLib: Option[String]) extends Requirement

object InlineJavascriptRequirement {
  def apply(req: InlineJavascriptRequirementImpl): InlineJavascriptRequirement = {
    val exprLib = translateOptionalArray(req.getExpressionLib).asOption
      .map(_.iterator.map(translateString).mkString("\n"))
    InlineJavascriptRequirement(exprLib)
  }
}

case class SchemaDefRequirement(typeDefinitions: Vector[CwlType.CwlSchemaType]) extends Requirement

object SchemaDefRequirement {
  def apply(req: SchemaDefRequirementImpl,
            typeAliases: Map[String, CwlType.CwlSchemaType]): SchemaDefRequirement = {
    val typeDefs: Vector[CwlType.CwlSchemaType] = req.getTypes.asScala.map {
      case schema: CommandInputArraySchemaImpl  => CwlType.apply(schema, typeAliases)
      case schema: CommandInputRecordSchemaImpl => CwlType.apply(schema, typeAliases)
      case schema: CommandInputEnumSchemaImpl   => CwlType.apply(schema)
      case other =>
        throw new Exception(s"unexpected type definition ${other}")
    }.toVector
    SchemaDefRequirement(typeDefs)
  }
}

case class LoadListingRequirement(value: Option[LoadListing.LoadListing]) extends Requirement

object LoadListingRequirement {
  def apply(req: LoadListingRequirementImpl): LoadListingRequirement = {
    LoadListingRequirement(translateOptional(req.getLoadListing).map(LoadListing.from))
  }
}

case class DockerRequirement(pullName: Option[String],
                             loadUri: Option[String],
                             importUri: Option[String],
                             dockerfile: Option[String],
                             imageId: Option[String],
                             outputDirectory: Option[String])
    extends Requirement

object DockerRequirement {
  def apply(req: DockerRequirementImpl): DockerRequirement = {
    DockerRequirement(
        translateOptional(req.getDockerPull).map(translateString),
        translateOptional(req.getDockerLoad).map(translateString),
        translateOptional(req.getDockerImport).map(translateString),
        translateOptional(req.getDockerFile).map(translateString),
        translateOptional(req.getDockerImageId).map(translateString),
        translateOptional(req.getDockerOutputDirectory).map(translateString)
    )
  }
}

case class SoftwarePackage(name: String, version: Vector[String], specs: Vector[String])

object SoftwarePackage {
  def apply(pkg: SoftwarePackageImpl): SoftwarePackage = {
    SoftwarePackage(pkg.getPackage,
                    translateOptionalArray(pkg.getVersion).map(translateString),
                    translateOptionalArray(pkg.getSpecs).map(translateString))
  }
}

case class SoftwareRequirement(packages: Vector[SoftwarePackage]) extends Requirement

object SoftwareRequirement {
  def apply(req: SoftwareRequirementImpl): SoftwareRequirement = {
    SoftwareRequirement(
        req.getPackages.asScala.map {
          case pkg: SoftwarePackageImpl => SoftwarePackage(pkg)
          case other =>
            throw new RuntimeException(s"unexpected SoftwarePackage value ${other}")
        }.toVector
    )
  }
}

case class InitialWorkDirRequirement(listing: Vector[CwlExpr]) extends Requirement

object InitialWorkDirRequirement {
  def apply(req: InitialWorkDirRequirementImpl): InitialWorkDirRequirement = {
    InitialWorkDirRequirement(translateArray(req.getListing).map(CwlExpr.apply))
  }
}

case class EnvironmentDefinition(name: String, value: CwlExpr)

object EnvironmentDefinition {
  def apply(env: EnvironmentDefImpl): EnvironmentDefinition = {
    EnvironmentDefinition(env.getEnvName, CwlExpr(env.getEnvValue))
  }
}

case class EnvVarRequirement(environmentDefinitions: Vector[EnvironmentDefinition])
    extends Requirement

object EnvVarRequirement {
  def apply(req: EnvVarRequirementImpl): EnvVarRequirement = {
    EnvVarRequirement(req.getEnvDef.asScala.map {
      case env: EnvironmentDefImpl => EnvironmentDefinition(env)
      case other =>
        throw new RuntimeException(s"unexpected EnvironmentDef value ${other}")
    }.toVector)
  }
}

case object ShellCommandRequirement extends Requirement

case class ResourceRequirement(coresMin: Option[CwlExpr],
                               coresMax: Option[CwlExpr],
                               ramMin: Option[CwlExpr],
                               ramMax: Option[CwlExpr],
                               tmpdirMin: Option[CwlExpr],
                               tmpdirMax: Option[CwlExpr],
                               outdirMin: Option[CwlExpr],
                               outdirMax: Option[CwlExpr])
    extends Requirement

object ResourceRequirement {
  def apply(req: ResourceRequirementImpl): ResourceRequirement = {
    ResourceRequirement(
        translateOptionalObject(req.getCoresMin).map(CwlExpr.apply),
        translateOptionalObject(req.getCoresMax).map(CwlExpr.apply),
        translateOptionalObject(req.getRamMin).map(CwlExpr.apply),
        translateOptionalObject(req.getRamMax).map(CwlExpr.apply),
        translateOptionalObject(req.getTmpdirMin).map(CwlExpr.apply),
        translateOptionalObject(req.getTmpdirMax).map(CwlExpr.apply),
        translateOptionalObject(req.getOutdirMin).map(CwlExpr.apply),
        translateOptionalObject(req.getOutdirMax).map(CwlExpr.apply)
    )
  }
}

case class WorkReuseRequirement(enable: CwlExpr) extends Requirement

object WorkReuseRequirement {
  def apply(req: WorkReuseImpl): WorkReuseRequirement = {
    WorkReuseRequirement(CwlExpr.apply(req.getEnableReuse))
  }
}

case class NetworkAccessRequirement(allow: CwlExpr) extends Requirement

object NetworkAccessRequirement {
  def apply(req: NetworkAccessImpl): NetworkAccessRequirement = {
    NetworkAccessRequirement(CwlExpr.apply(req.getNetworkAccess))
  }
}

case class InplaceUpdateRequirement(allow: Boolean) extends Requirement

object InplaceUpdateRequirement {
  def apply(req: InplaceUpdateRequirementImpl): InplaceUpdateRequirement = {
    InplaceUpdateRequirement(req.getInplaceUpdate)
  }
}

case class ToolTimeLimitRequirement(timeLimit: CwlExpr) extends Requirement

object ToolTimeLimitRequirement {
  def apply(req: ToolTimeLimitImpl): ToolTimeLimitRequirement = {
    ToolTimeLimitRequirement(CwlExpr.apply(req.getTimelimit))
  }
}

sealed trait Argument
case class ExprArgument(expr: CwlExpr) extends Argument
case class BindingArgument(binding: CommandInputBinding) extends Argument

object Requirement {
  def apply(requirement: ProcessRequirement): Requirement = {
    requirement match {
      case req: InlineJavascriptRequirementImpl => InlineJavascriptRequirement(req)
      case req: SchemaDefRequirementImpl        => SchemaDefRequirement(req, Map.empty)
      case req: LoadListingRequirementImpl      => LoadListingRequirement(req)
      case req: DockerRequirementImpl           => DockerRequirement(req)
      case req: SoftwareRequirementImpl         => SoftwareRequirement(req)
      case req: InitialWorkDirRequirementImpl   => InitialWorkDirRequirement(req)
      case req: EnvVarRequirementImpl           => EnvVarRequirement(req)
      case _: ShellCommandRequirementImpl       => ShellCommandRequirement
      case req: ResourceRequirementImpl         => ResourceRequirement(req)
      case req: WorkReuseImpl                   => WorkReuseRequirement(req)
      case req: NetworkAccessImpl               => NetworkAccessRequirement(req)
      case req: InplaceUpdateRequirementImpl    => InplaceUpdateRequirement(req)
      case req: ToolTimeLimitImpl               => ToolTimeLimitRequirement(req)
      case _ =>
        throw new RuntimeException(s"unsupported requirement value ${requirement}")
    }
  }
}

sealed trait DocumentElement {
  val source: Option[String]
}

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
                           stdin: Option[CwlExpr],
                           stdout: Option[CwlExpr],
                           stderr: Option[CwlExpr],
                           requirements: Vector[Requirement],
                           hints: Vector[Requirement],
                           successCodes: Set[Int],
                           temporaryFailCodes: Set[Int],
                           permanentFailCodes: Set[Int])
    extends DocumentElement

object CommandLineTool {
  def apply(tool: CommandLineToolImpl, source: Option[String] = None): CommandLineTool = {
    val requirements = translateOptionalArray(tool.getRequirements).map {
      case req: ProcessRequirement => Requirement(req)
      case other =>
        throw new RuntimeException(s"unexpected requirement value ${other}")
    }

    val typeAliases: Map[String, CwlType.CwlSchemaType] = requirements
      .collect {
        case SchemaDefRequirement(typeDefs) => typeDefs
      }
      .flatten
      .map(schemaDef => schemaDef.name.get -> schemaDef)
      .toMap

    val (inputParams, isStdin) = tool.getInputs.asScala
      .map {
        case param: CommandInputParameterImpl => CommandInputParameter(param, typeAliases)
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
    val toolStdin: Option[CwlExpr] = translateOptionalObject(tool.getStdin).map(CwlExpr.apply)
    if (paramStdin.isDefined && toolStdin.isDefined) {
      throw new RuntimeException(
          s"'stdin' specified at both the tool level and by parameter ${paramStdin.get}"
      )
    }
    val stdin = paramStdin
      .map { param =>
        CwlExpr(s"$${inputs.${param.id}.path}")
      }
      .orElse(toolStdin)

    val (outputParams, stdfile) = tool.getOutputs.asScala
      .map {
        case param: CommandOutputParameterImpl => CommandOutputParameter(param, typeAliases)
        case other =>
          throw new RuntimeException(s"unexpected CommandOutputParameter value ${other}")
      }
      .toVector
      .unzip

    val stdout: Option[CwlExpr] =
      translateOptionalObject(tool.getStdout).map(CwlExpr.apply) match {
        case None if stdfile.contains(Some(StdFile.Stdout)) =>
          Some(CwlExpr.RandomFile(StdFile.Stdout))
        case other => other
      }
    val stderr: Option[CwlExpr] =
      translateOptionalObject(tool.getStderr).map(CwlExpr.apply) match {
        case None if stdfile.contains(Some(StdFile.Stderr)) =>
          Some(CwlExpr.RandomFile(StdFile.Stderr))
        case other => other
      }

    val arguments = translateOptionalArray(tool.getArguments).map {
      case binding: CommandLineBindingImpl => BindingArgument(CommandInputBinding(binding))
      case expr                            => ExprArgument(CwlExpr.apply(expr))
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

case class Workflow(source: Option[String]) extends DocumentElement

object Workflow {
  def apply(workflow: WorkflowImpl, source: Option[String] = None): Workflow = {
    throw new NotImplementedError(s"translateWorkflow not implemented; can't translate ${workflow}")
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
