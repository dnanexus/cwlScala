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

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

/**
  * Marker trait for all CWL data types.
  */
sealed trait CwlType {

  /**
    * Returns true if this type is coercible to the specified type
    */
  def coercibleTo(targetType: CwlType): Boolean = {
    val nonOptType = CwlOptional.unwrapOptional(targetType)
    Set[CwlType](this, CwlAny).contains(nonOptType) || canBeCoercedTo(nonOptType)
  }

  /**
    * Returns true if this type can be coerced to targetType,
    * which is a non-optional, non-equal, and non-Any type.
    */
  protected def canBeCoercedTo(targetType: CwlType): Boolean = false
}

object CwlType {

  /**
    * Translates a Java type object to a [[CwlType]]. Since CWL allows parameters
    * to be polymorphic (i.e. accept multiple types), the value returned is a
    * [[Vector[CwlType]]]. If the type is any of `stdin`, `stdout`, `stderr`, it is
    * converted to a [[CwlFile]] and a [[Some(StdFile.StdFile)]] is also returned
    * indicating which std file is represented.
    *
    * TODO: we need to handle optional types, but they currently fail to parse due to
    *  https://github.com/common-workflow-lab/cwljava/issues/29
    *
    * @param t the Java type object - may be a [[java.util.List]] of multiple types
    * @param schemaDefs schema definitions to use for resolving non-standard types
    * @return a tuple [[(Vector[CwlType], Option[StdFile.StdFile])]].
    */
  def apply(
      t: java.lang.Object,
      schemaDefs: Map[String, CwlSchema] = Map.empty
  ): (Vector[CwlType], Option[StdFile.StdFile]) = {
    t match {
      case a: java.util.List[_] =>
        val (cwlTypes, stdfile) =
          a.asInstanceOf[java.util.List[java.lang.Object]].asScala.map(apply(_, schemaDefs)).unzip
        val stdfiles = stdfile.flatten.toSet
        if (stdfiles.size > 1) {
          throw new Exception(s"found multiple different std types ${stdfiles.mkString(",")}")
        }
        (cwlTypes.flatten.toVector, stdfiles.headOption)
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
          case "Any"                        => CwlAny
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
            throw new RuntimeException(s"unexpected type ${other}")
        }
        (Vector(cwlType), None)
    }
  }
}

case object CwlNull extends CwlType

case object CwlAny extends CwlType {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType != CwlNull
  }
}

/**
  * An optional type.
  * @param t the inner type
  * @example {{{string?}}} is translated to {{{CwlOptional(CwlString)}}}
  */
case class CwlOptional(t: CwlType) extends CwlType {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    CwlNull == targetType
  }
}

object CwlOptional {
  def isOptional(t: CwlType): Boolean = {
    t match {
      case CwlOptional(_) => true
      case _              => false
    }
  }

  def anyOptional(types: Vector[CwlType]): Boolean = {
    types.exists(isOptional)
  }

  @tailrec
  def unwrapOptional(t: CwlType): CwlType = {
    t match {
      case CwlOptional(innerType) => unwrapOptional(innerType)
      case _                      => t
    }
  }

  def ensureOptional(t: CwlType): CwlType = {
    t match {
      case optType: CwlOptional => optType
      case _                    => CwlOptional(t)
    }
  }
}

/**
  * All valid CWL types are primitive, excepting `Any`, `null`, and schema types.
  */
sealed trait CwlPrimitive extends CwlType {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    CwlString == targetType
  }
}

case object CwlBoolean extends CwlPrimitive

case object CwlString extends CwlPrimitive {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case _: CwlPrimitive => true
      case _               => false
    }
  }
}

/**
  * Parent trait of the four CWL numeric types
  */
sealed trait CwlNumber extends CwlPrimitive {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case _: CwlNumber => true
      case CwlString    => true
      case _            => false
    }
  }
}

case object CwlInt extends CwlNumber
case object CwlLong extends CwlNumber
case object CwlFloat extends CwlNumber
case object CwlDouble extends CwlNumber

/**
  * Parent trait of the two CWL path types
  */
sealed trait CwlPath extends CwlPrimitive {
  def className: String
}
case object CwlFile extends CwlPath {
  val className: String = "File"
}
case object CwlDirectory extends CwlPath {
  val className: String = "Directory"
}

/**
  * Parent of CWL schema types. Note that input and output schema definitions
  * both use the same objects, but `inputBinding` will always be `None` for
  * output types.
  */
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
    schema match {
      case arraySchema: CommandOutputArraySchemaImpl   => CwlArray(arraySchema, schemaDefs)
      case recordSchema: CommandOutputRecordSchemaImpl => CwlRecord(recordSchema, schemaDefs)
      case enumSchema: CommandOutputEnumSchemaImpl     => CwlEnum(enumSchema)
      case _ =>
        throw new Exception(s"unexpected output schema ${schema}")
    }
  }
}

case class CwlArray(itemTypes: Vector[CwlType],
                    name: Option[String] = None,
                    label: Option[String] = None,
                    doc: Option[String] = None,
                    inputBinding: Option[CommandInputBinding] = None)
    extends CwlSchema {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
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
                          types: Vector[CwlType],
                          label: Option[String] = None,
                          doc: Option[String] = None,
                          inputBinding: Option[CommandInputBinding] = None,
                          secondaryFiles: Vector[SecondaryFile] = Vector.empty,
                          format: Vector[CwlValue] = Vector.empty,
                          streamable: Option[Boolean] = None,
                          loadContents: Option[Boolean] = None,
                          loadListing: Option[LoadListing.LoadListing] = None) {

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
                     name: Option[String] = None,
                     label: Option[String] = None,
                     doc: Option[String] = None,
                     inputBinding: Option[CommandInputBinding] = None)
    extends CwlSchema {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
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
        types,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
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
        types,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
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
            schemaDefs: Map[String, CwlSchema]): CwlRecord = {
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
                   name: Option[String] = None,
                   label: Option[String] = None,
                   doc: Option[String] = None,
                   inputBinding: Option[CommandInputBinding] = None)
    extends CwlSchema {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
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
