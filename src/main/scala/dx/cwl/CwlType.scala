package dx.cwl

import dx.cwl.Utils.{translateDoc, translateOptional, translateOptionalArray}
import org.w3id.cwl.cwl1_2.{
  CWLType,
  CommandInputArraySchemaImpl,
  CommandInputEnumSchemaImpl,
  CommandInputRecordFieldImpl,
  CommandInputRecordSchemaImpl,
  CommandInputSchema,
  CommandLineBindingImpl,
  CommandOutputArraySchemaImpl,
  CommandOutputEnumSchemaImpl,
  CommandOutputRecordFieldImpl,
  CommandOutputRecordSchemaImpl,
  OutputSchema,
  SecondaryFileSchemaImpl,
  stderr => CWLStderr,
  stdin => CWLStdin,
  stdout => CWLStdout
}

import scala.jdk.CollectionConverters._

sealed trait CwlType {
  def coercibleTo(targetType: CwlType): Boolean
}

object CwlType {
  // TODO: we need to handle optional types, but they currently fail to parse due to
  //  https://github.com/common-workflow-lab/cwljava/issues/29
  def apply(
      t: java.lang.Object,
      schemaDefs: Map[String, CwlSchema]
  ): (Vector[CwlType], Option[StdFile.StdFile]) = {
    t match {
      case a: java.util.List[_] =>
        val (cwlTypes, stdfile) =
          a.asInstanceOf[java.util.List[java.lang.Object]].asScala.map(apply(_, schemaDefs)).unzip
        (cwlTypes.flatten.toVector, stdfile.flatten.toSet.headOption)
      case CWLStdin.STDIN   => (Vector(CwlFile), Some(StdFile.Stdin))
      case CWLStdout.STDOUT => (Vector(CwlFile), Some(StdFile.Stdout))
      case CWLStderr.STDERR => (Vector(CwlFile), Some(StdFile.Stderr))
      case _ =>
        val cwlType: CwlType = t match {
          case "string"                     => CwlString
          case "boolean"                    => CwlBoolean
          case "int"                        => CwlInt
          case "long"                       => CwlLong
          case "float"                      => CwlFloat
          case "double"                     => CwlDouble
          case "null"                       => CwlNull
          case CWLType.FILE                 => CwlFile
          case CWLType.DIRECTORY            => CwlDirectory
          case schema: CommandInputSchema   => CwlSchema(schema, schemaDefs)
          case schema: OutputSchema         => CwlSchema(schema, schemaDefs)
          case s: String if s.contains("#") =>
            // a schema reference
            val schemaName = s.drop(s.indexOf("#") + 1)
            schemaDefs.get(schemaName) match {
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

sealed trait CwlPrimitive extends CwlType {
  def coercibleTypes: Set[CwlType]

  override def coercibleTo(targetType: CwlType): Boolean = {
    this == targetType || coercibleTypes.contains(targetType)
  }
}

case object CwlString extends CwlPrimitive {
  override val coercibleTypes: Set[CwlType] =
    Set(CwlBoolean, CwlInt, CwlLong, CwlFloat, CwlDouble, CwlFile, CwlDirectory)
}

case object CwlBoolean extends CwlPrimitive {
  override val coercibleTypes: Set[CwlType] = Set(CwlString)
}

sealed trait CwlNumber extends CwlPrimitive {
  override val coercibleTypes: Set[CwlType] = Set(CwlInt, CwlLong, CwlFloat, CwlDouble, CwlString)
}

case object CwlInt extends CwlNumber
case object CwlLong extends CwlNumber
case object CwlFloat extends CwlNumber
case object CwlDouble extends CwlNumber

sealed trait CwlPath extends CwlPrimitive {
  override val coercibleTypes: Set[CwlType] = Set(CwlString)
}

case object CwlFile extends CwlPath
case object CwlDirectory extends CwlPath

case object CwlNull extends CwlPrimitive {
  override val coercibleTypes: Set[CwlType] = Set.empty[CwlType]
}

case class CwlOptional(t: CwlType) extends CwlPrimitive {
  override val coercibleTypes: Set[CwlType] = Set(CwlNull)
}

sealed trait CwlSchema extends CwlType {
  val name: Option[String]
  val label: Option[String]
  val doc: Option[String]
  val inputBinding: Option[CommandInputBinding]
}

object CwlSchema {
  def apply(schema: CommandInputSchema, schemaDefs: Map[String, CwlSchema]): CwlSchema = {
    schema match {
      case schema: CommandInputArraySchemaImpl  => CwlArray(schema, schemaDefs)
      case schema: CommandInputRecordSchemaImpl => CwlRecord(schema, schemaDefs)
      case schema: CommandInputEnumSchemaImpl   => CwlEnum(schema, schemaDefs)
      case _ =>
        throw new Exception(s"unexpected input schema ${schema}")
    }
  }

  def apply(schema: OutputSchema, schemaDefs: Map[String, CwlSchema]): CwlSchema = {
    case schema: CommandOutputArraySchemaImpl  => CwlArray(schema, schemaDefs)
    case schema: CommandOutputRecordSchemaImpl => CwlRecord(schema, schemaDefs)
    case schema: CommandOutputEnumSchemaImpl   => CwlEnum(schema)
    case _ =>
      throw new Exception(s"unexpected output schema ${schema}")
  }
}

case class CwlArray(itemTypes: Vector[CwlType],
                    name: Option[String],
                    label: Option[String],
                    doc: Option[String],
                    inputBinding: Option[CommandInputBinding])
    extends CwlSchema {
  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlArray if targetSchema == this => true
      case targetSchema: CwlArray =>
        itemTypes.exists { fromType =>
          targetSchema.itemTypes.exists { toType =>
            fromType.coercibleTo(toType)
          }
        }
      case _ => false
    }
  }
}

object CwlArray {
  def apply(schema: CommandInputArraySchemaImpl, schemaDefs: Map[String, CwlSchema]): CwlArray = {
    val (types, stdfile) = CwlType(schema.getItems, schemaDefs)
    assert(stdfile.isEmpty)
    CwlArray(
        types,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
    )
  }

  def apply(schema: CommandOutputArraySchemaImpl, schemaDefs: Map[String, CwlSchema]): CwlArray = {
    val (types, stdfile) = CwlType(schema.getItems, schemaDefs)
    assert(stdfile.isEmpty)
    CwlArray(
        types,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        None
    )
  }
}

case class CwlRecordField(name: String,
                          label: Option[String],
                          doc: Option[String],
                          types: Vector[CwlType],
                          inputBinding: Option[CommandInputBinding],
                          secondaryFiles: Vector[SecondaryFile],
                          format: Vector[CwlValue],
                          streamable: Option[Boolean],
                          loadContents: Option[Boolean],
                          loadListing: Option[LoadListing.LoadListing]) {

  /**
    * the field is optional if any of the allowed types are optional
    */
  lazy val optional: Boolean = {
    types.exists {
      case CwlOptional(_) => true
      case _              => false
    }
  }
}

case class CwlRecord(fields: Map[String, CwlRecordField],
                     name: Option[String],
                     label: Option[String],
                     doc: Option[String],
                     inputBinding: Option[CommandInputBinding])
    extends CwlSchema {
  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlRecord if this == targetSchema => true
      case targetSchema: CwlRecord if fields.keySet == targetSchema.fields.keySet =>
        fields.forall {
          case (name, fromField) =>
            fromField.types.exists { fromType =>
              targetSchema.fields(name).types.exists { toType =>
                fromType.coercibleTo(toType)
              }
            }
        }
      case _ => false
    }
  }
}

object CwlRecord {
  def apply(field: CommandInputRecordFieldImpl,
            schemaDefs: Map[String, CwlSchema]): CwlRecordField = {
    val (types, stdfile) = CwlType(field.getType, schemaDefs)
    assert(stdfile.isEmpty)
    CwlRecordField(
        field.getName,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
        types,
        translateOptional(field.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        },
        translateOptionalArray(field.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(field.getFormat).map(CwlValue.apply(_, schemaDefs)),
        translateOptional(field.getStreamable).map(_.booleanValue()),
        translateOptional(field.getLoadContents).map(_.booleanValue()),
        translateOptional(field.getLoadListing).map(LoadListing.from)
    )
  }

  def apply(schema: CommandInputRecordSchemaImpl, schemaDefs: Map[String, CwlSchema]): CwlRecord = {
    CwlRecord(
        translateOptional(schema.getFields)
          .map(_.asScala.map {
            case field: CommandInputRecordFieldImpl =>
              val cwlField = apply(field, schemaDefs)
              cwlField.name -> cwlField
            case other =>
              throw new RuntimeException(s"invalid record field ${other}")
          }.toMap)
          .getOrElse(Map.empty),
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
    )
  }

  def apply(field: CommandOutputRecordFieldImpl,
            schemaDefs: Map[String, CwlSchema]): CwlRecordField = {
    val (types, stdfile) = CwlType(field.getType, schemaDefs)
    assert(stdfile.isEmpty)
    CwlRecordField(
        field.getName,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
        types,
        None,
        translateOptionalArray(field.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(field.getFormat).map(CwlValue.apply(_, schemaDefs)),
        translateOptional(field.getStreamable).map(_.booleanValue()),
        None,
        None
    )
  }

  def apply(schema: CommandOutputRecordSchemaImpl,
            schemaDefs: Map[String, CwlSchema] = Map.empty): CwlRecord = {
    CwlRecord(
        translateOptional(schema.getFields)
          .map(_.asScala.map {
            case field: CommandOutputRecordFieldImpl =>
              val cwlField = apply(field, schemaDefs)
              cwlField.name -> cwlField
            case other =>
              throw new RuntimeException(s"invalid record field ${other}")
          }.toMap)
          .getOrElse(Map.empty),
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        None
    )
  }
}

case class CwlEnum(symbols: Set[String],
                   name: Option[String],
                   label: Option[String],
                   doc: Option[String],
                   inputBinding: Option[CommandInputBinding])
    extends CwlSchema {
  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlEnum if this.symbols == targetSchema.symbols => true
      case CwlString                                                     => true
      case _                                                             => false
    }
  }
}

object CwlEnum {
  def apply(schema: CommandInputEnumSchemaImpl, schemaDefs: Map[String, CwlSchema]): CwlEnum = {
    CwlEnum(
        schema.getSymbols.asScala.map {
          case s: String => s
          case other     => throw new Exception(s"unexpected symbol value ${other}")
        }.toSet,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        translateOptional(schema.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
    )
  }

  def apply(schema: CommandOutputEnumSchemaImpl): CwlEnum = {
    CwlEnum(
        schema.getSymbols.asScala.map {
          case s: String => s
          case other     => throw new Exception(s"unexpected symbol value ${other}")
        }.toSet,
        translateOptional(schema.getName),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        None
    )
  }
}
