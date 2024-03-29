package dx.cwl

import dx.cwl.Utils.{translateDoc, translateOptional, translateOptionalArray}
import org.w3id.cwl.cwl1_2.{
  ArraySchema,
  CWLType,
  CommandInputArraySchema,
  CommandInputEnumSchema,
  CommandInputRecordField,
  CommandInputRecordFieldImpl,
  CommandInputRecordSchema,
  CommandLineBindingImpl,
  CommandOutputBindingImpl,
  CommandOutputRecordField,
  CommandOutputRecordFieldImpl,
  EnumSchema,
  IOSchema,
  InputArraySchema,
  InputEnumSchema,
  InputRecordField,
  InputRecordSchema,
  InputSchema,
  OutputArraySchema,
  OutputEnumSchema,
  OutputRecordField,
  OutputRecordSchema,
  OutputSchema,
  SecondaryFileSchemaImpl,
  stderr => CWLStderr,
  stdin => CWLStdin,
  stdout => CWLStdout
}

import scala.annotation.tailrec
import scala.collection.immutable.{SeqMap, TreeSeqMap}
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

/**
  * Marker trait for all CWL data types.
  */
sealed trait CwlType extends Equals {

  /**
    * Returns true if this type is coercible to the specified type
    */
  def coercibleTo(targetType: CwlType): Boolean
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
  private[cwl] def translateRaw(
      t: java.lang.Object,
      schemaDefs: Map[String, CwlSchema] = Map.empty,
      rawSchemaDefs: Map[String, IOSchema] = Map.empty
  ): (CwlType, Option[StdFile.StdFile], Map[String, CwlSchema]) = {
    def inner(
        innerType: java.lang.Object,
        innerSchemaDefs: Map[String, CwlSchema]
    ): (Vector[CwlType], Option[StdFile.StdFile], Map[String, CwlSchema]) = {
      innerType match {
        case a: java.util.List[_] =>
          a.asInstanceOf[java.util.List[java.lang.Object]]
            .asScala
            .foldLeft(Vector.empty[CwlType], Option.empty[StdFile.StdFile], innerSchemaDefs) {
              case ((cwlTypes, stdfile, newSchemaDefs), t) =>
                val (newTypes, newStdfile, updatedSchemaDefs) = inner(t, newSchemaDefs)
                val updatedStdfile = (stdfile, newStdfile) match {
                  case (None, None)         => None
                  case (s, None)            => s
                  case (None, s)            => s
                  case (s1, s2) if s1 == s2 => s1
                  case (s1, s2) =>
                    throw new RuntimeException(s"found multiple different std types ${s1},${s2}")
                }
                (cwlTypes ++ newTypes, updatedStdfile, updatedSchemaDefs)
            }
        case CWLStdin.STDIN   => (Vector(CwlFile), Some(StdFile.Stdin), innerSchemaDefs)
        case CWLStdout.STDOUT => (Vector(CwlFile), Some(StdFile.Stdout), innerSchemaDefs)
        case CWLStderr.STDERR => (Vector(CwlFile), Some(StdFile.Stderr), innerSchemaDefs)
        case schema: IOSchema =>
          val (newType, newSchemaDefs) =
            CwlSchema.translateSchema(schema, innerSchemaDefs, rawSchemaDefs)
          val updatedSchemaDefs = if (newType.hasName) {
            innerSchemaDefs ++ newSchemaDefs + (newType.frag -> newType)
          } else {
            innerSchemaDefs ++ newSchemaDefs
          }
          (Vector(newType), None, updatedSchemaDefs)
        case schemaName: String if schemaName.contains("#") =>
          // a schema reference
          val id = Identifier.parse(schemaName)
          val fqn = id.fullyQualifiedName
          val schemaDef = schemaDefs
            .get(fqn)
            .orElse(schemaDefs.get(id.frag))
            .orElse(innerSchemaDefs.get(fqn))
            .orElse(innerSchemaDefs.get(id.frag))
          schemaDef match {
            case Some(schemaDef) => (Vector(schemaDef), None, innerSchemaDefs)
            case None if rawSchemaDefs.contains(fqn) || rawSchemaDefs.contains(id.frag) =>
              val rawSchemaDef = rawSchemaDefs.getOrElse(fqn, rawSchemaDefs(id.frag))
              val (types, stdfile, updatedSchemaDefs) = inner(rawSchemaDef, innerSchemaDefs)
              val newSchemaDef = types match {
                case Vector(s: CwlSchema) => s
                case other =>
                  throw new RuntimeException(s"expected single CwlSchema, not ${other}")
              }
              (types, stdfile, updatedSchemaDefs + (fqn -> newSchemaDef))
            case None =>
              throw new RuntimeException(s"missing definition for schema ${fqn}")
          }
        case _ =>
          val cwlType: CwlType = innerType match {
            case "string"          => CwlString
            case "boolean"         => CwlBoolean
            case "int"             => CwlInt
            case "long"            => CwlLong
            case "float"           => CwlFloat
            case "double"          => CwlDouble
            case "null"            => CwlNull
            case "Any"             => CwlAny
            case CWLType.FILE      => CwlFile
            case CWLType.DIRECTORY => CwlDirectory
            case other =>
              throw new RuntimeException(s"unexpected type ${other}")
          }
          (Vector(cwlType), None, innerSchemaDefs)
      }
    }
    val (types, stdFile, updatedSchemas) = inner(t, Map.empty)
    (flatten(types), stdFile, updatedSchemas)
  }

  /**
    * Simplifies a Set of alternative types
    * 1) if CwlNull is in the set, convert all types to optional and remove CwlNull
    * 2) if CwlAny or CwlOptional(CwlAny) is in the set, reduce the set to that
    * single type (since Any is a superset of all non-null types)
    * @param types set of alternative types
    * @return
    */
  def flatten(types: Vector[CwlType]): CwlType = {
    @tailrec
    def inner(innerTypes: Set[CwlType]): Set[CwlType] = {
      if (innerTypes.size > 1 && innerTypes.contains(CwlNull)) {
        inner(innerTypes.diff(Set(CwlNull)).map(CwlOptional.ensureOptional))
      } else if (innerTypes.contains(CwlOptional(CwlAny))) {
        Set(CwlOptional(CwlAny))
      } else if (innerTypes.contains(CwlAny)) {
        Set(CwlAny)
      } else {
        innerTypes
      }
    }
    val flattened = inner(types.toSet)
    if (flattened.size == 1) {
      flattened.head
    } else {
      CwlMulti(flattened.toVector)
    }
  }

  def translate(
      t: java.lang.Object,
      schemaDefs: Map[String, CwlSchema] = Map.empty
  ): (CwlType, Option[StdFile.StdFile]) = {
    val (cwlType, stdfile, _) = translateRaw(t, schemaDefs, Map.empty)
    (cwlType, stdfile)
  }

  def copySimplifyIds(cwlType: CwlType,
                      dropNamespace: Boolean,
                      replacePrefix: (Either[Boolean, String], Option[String]),
                      simplifyAutoNames: Boolean,
                      dropCwlExtension: Boolean): CwlType = {
    cwlType match {
      case i: Identifiable =>
        i.copySimplifyIds(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension) match {
          case t: CwlType => t
          case other      => throw new Exception(s"expected CwlType, not ${other}")
        }
      case CwlOptional(innerType) =>
        CwlOptional(
            copySimplifyIds(innerType,
                            dropNamespace,
                            replacePrefix,
                            simplifyAutoNames,
                            dropCwlExtension)
        )
      case CwlMulti(types) =>
        CwlMulti(
            types.map(
                copySimplifyIds(_,
                                dropNamespace,
                                replacePrefix,
                                simplifyAutoNames,
                                dropCwlExtension)
            )
        )
      case _ => cwlType
    }
  }
}

case object CwlNull extends CwlType {
  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case CwlNull | CwlOptional(_)            => true
      case multi: CwlMulti if multi.isOptional => true
      case _                                   => false
    }
  }
}

case object CwlAny extends CwlType {
  override def coercibleTo(targetType: CwlType): Boolean = {
    CwlOptional.unwrapOptional(targetType) != CwlNull
  }
}

/**
  * An optional type.
  * @param t the inner type
  * @example {{{string?}}} is translated to {{{CwlOptional(CwlString)}}}
  */
case class CwlOptional(t: CwlType) extends CwlType {
  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case CwlAny | CwlNull | CwlOptional(CwlNull) => true
      case CwlOptional(other)                      => t.coercibleTo(other)
      case multi: CwlMulti if multi.isOptional     => multi.types.exists(coercibleTo)
      case _                                       => false
    }
  }

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[CwlOptional]

  override def equals(that: Any): Boolean =
    that match {
      case obj: CwlOptional =>
        obj match {
          case obj if this eq obj => true
          case obj
              if obj.canEqual(this)
                && (hashCode == obj.hashCode)
                && (CwlOptional.unwrapOptional(this) == CwlOptional.unwrapOptional(obj)) =>
            true
          case _ => false
        }
      case _ =>
        false
    }

  override def hashCode(): Int =
    31 * CwlOptional.unwrapOptional(this).hashCode()
}

object CwlOptional {
  def isOptional(t: CwlType): Boolean = {
    t match {
      case CwlOptional(_)  => true
      case multi: CwlMulti => multi.isOptional
      case _               => false
    }
  }

  def unwrapOptional(t: CwlType): CwlType = {
    t match {
      case CwlOptional(innerType) => unwrapOptional(innerType)
      case CwlMulti(types)        => CwlMulti(types.map(unwrapOptional))
      case _                      => t
    }
  }

  def ensureOptional(t: CwlType): CwlType = {
    t match {
      case optType: CwlOptional => optType
      case CwlMulti(types)      => CwlMulti(types.map(ensureOptional))
      case _                    => CwlOptional(t)
    }
  }
}

/**
  * A meta-type that matches any one of several types.
  */
case class CwlMulti(types: Vector[CwlType]) extends CwlType {
  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case CwlMulti(otherTypes) => otherTypes.exists(coercibleTo)
      case _                    => types.exists(_.coercibleTo(targetType))
    }
  }

  lazy val isOptional: Boolean = types.exists(CwlOptional.isOptional)

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[CwlMulti]

  override def equals(that: Any): Boolean = {
    def multiEqual(v1: Vector[CwlType], v2: Vector[CwlType]): Boolean = {
      if (v1.size != v2.size)
        false
      else {
        v1.sortBy(_.toString()).zip(v2.sortBy(_.toString())).forall(p => p._1 == p._2)
      }
    }

    that match {
      case m: CwlMulti =>
        m match {
          case m if this eq m => true
          case m
              if m.canEqual(this)
                && (hashCode == m.hashCode)
                && multiEqual(types, m.types) =>
            true
          case _ => false
        }
      case _ =>
        false
    }
  }

  override def hashCode(): Int =
    31 * types.map(_.hashCode()).sum

}

/**
  * All valid CWL types are primitive, excepting `Any`, `null`, and schema types.
  */
sealed trait CwlPrimitive extends CwlType {
  def coercibleTo(targetType: CwlType): Boolean = {
    val nonOptType = CwlOptional.unwrapOptional(targetType)
    nonOptType == this || (nonOptType match {
      case CwlAny          => true
      case CwlMulti(types) => types.exists(coercibleTo)
      case _               => canBeCoercedTo(nonOptType)
    })
  }

  /**
    * Returns true if this type can be coerced to targetType,
    * which is a non-optional, non-equal, non-Any, and
    * non-Multi type.
    */
  protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    CwlString == targetType
  }
}

case object CwlBoolean extends CwlPrimitive

case object CwlString extends CwlPrimitive {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case _: CwlPrimitive => true
      case _: CwlEnum      => true
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
sealed trait CwlSchema extends CwlType with Identifiable {
  val label: Option[String]
  val doc: Option[String]

  def coercibleTo(targetType: CwlType): Boolean = {
    val nonOptType = CwlOptional.unwrapOptional(targetType)
    nonOptType == this || (nonOptType match {
      case CwlAny          => true
      case CwlMulti(types) => types.exists(coercibleTo)
      case _               => canBeCoercedTo(nonOptType)
    })
  }

  /**
    * Returns true if this type can be coerced to targetType,
    * which is a non-optional, non-equal, and non-Any type.
    */
  protected def canBeCoercedTo(targetType: CwlType): Boolean = false

  def hasRandomName(): Boolean = {
    val randomNameRegex: Regex = "([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})".r
    id.map(_.name) match {
      case Some(randomNameRegex(_)) => true
      case Some(name)               => false
      case None                     => true
    }
  }
}

sealed trait CwlInputSchema extends CwlSchema {
  val inputBinding: Option[CommandInputBinding]
}

object CwlSchema {
  def apply(schema: InputSchema, schemaDefs: Map[String, CwlSchema]): CwlSchema = {
    schema match {
      case schema: CommandInputArraySchema  => CwlArray(schema, schemaDefs)
      case schema: InputArraySchema         => CwlArray(schema, schemaDefs)
      case schema: CommandInputRecordSchema => CwlInputRecord(schema, schemaDefs)
      case schema: InputRecordSchema        => CwlInputRecord(schema, schemaDefs)
      case schema: CommandInputEnumSchema   => CwlEnum(schema, schemaDefs)
      case schema: InputEnumSchema          => CwlEnum(schema, schemaDefs)
      case _ =>
        throw new Exception(s"unexpected input schema ${schema}")
    }
  }

  def apply(schema: OutputSchema,
            schemaDefs: Map[String, CwlSchema],
            rawSchemaDefs: Map[String, OutputSchema] = Map.empty): CwlSchema = {
    schema match {
      case schema: OutputArraySchema  => CwlArray(schema, schemaDefs)
      case schema: OutputRecordSchema => CwlOutputRecord(schema, schemaDefs)
      case schema: OutputEnumSchema   => CwlEnum(schema, schemaDefs)
      case _ =>
        throw new Exception(s"unexpected output schema ${schema}")
    }
  }

  def translateSchema(
      schema: IOSchema,
      schemaDefs: Map[String, CwlSchema],
      rawSchemaDefs: Map[String, IOSchema]
  ): (CwlSchema, Map[String, CwlSchema]) = {
    schema match {
      case arraySchema: ArraySchema =>
        CwlArray.translate(arraySchema, schemaDefs, rawSchemaDefs)
      case recordSchema: InputRecordSchema =>
        CwlInputRecord.translate(recordSchema, schemaDefs, rawSchemaDefs)
      case recordSchema: OutputRecordSchema =>
        CwlOutputRecord.translate(recordSchema, schemaDefs, rawSchemaDefs)
      case enumSchema: EnumSchema =>
        (CwlEnum(enumSchema, schemaDefs), Map.empty)
      case _ =>
        throw new Exception(s"unexpected input schema ${schema}")
    }
  }

  def translateSchemas(schemas: Map[String, IOSchema],
                       schemaDefs: Map[String, CwlSchema]): Map[String, CwlSchema] = {
    schemas.foldLeft(Map.empty[String, CwlSchema]) {
      case (accu, (name, _)) if accu.contains(name) || schemaDefs.contains(name) => accu
      case (accu, (name, schema)) =>
        val id = Identifier.parse(name)
        val fqn = id.fullyQualifiedName
        if (accu.contains(fqn) || schemaDefs.contains(fqn)) {
          accu
        } else {
          val (newSchema, newSchemaDefs) = translateSchema(schema, schemaDefs ++ accu, schemas)
          val newSchemaEntry = newSchema.id
            .map(_.fullyQualifiedName -> newSchema)
            .getOrElse(name -> newSchema)
          accu ++ newSchemaDefs + newSchemaEntry
        }
    }
  }

  def translateAll(schemas: Vector[java.lang.Object],
                   schemaDefs: Map[String, CwlSchema]): Map[String, CwlSchema] = {
    // schemas may reference each other, so first we build a maps of
    // name -> schema for input and output schema types, then recursively
    // translate each schema
    val (inputSchemas, outputSchemas) = schemas
      .foldLeft(Map.empty[String, InputSchema], Map.empty[String, OutputSchema]) {
        case ((i, o), schema: InputArraySchema) =>
          translateOptional(schema.getName) match {
            case Some(name) => (i + (name -> schema), o)
            case _          => (i, o)
          }
        case ((i, o), schema: InputRecordSchema) =>
          translateOptional(schema.getName) match {
            case Some(name) => (i + (name -> schema), o)
            case _          => (i, o)
          }
        case ((i, o), schema: InputEnumSchema) =>
          translateOptional(schema.getName) match {
            case Some(name) => (i + (name -> schema), o)
            case _          => (i, o)
          }
        case ((i, o), schema: OutputArraySchema) =>
          translateOptional(schema.getName) match {
            case Some(name) => (i, o + (name -> schema))
            case _          => (i, o)
          }
        case ((i, o), schema: OutputRecordSchema) =>
          translateOptional(schema.getName) match {
            case Some(name) => (i, o + (name -> schema))
            case _          => (i, o)
          }
        case ((i, o), schema: OutputEnumSchema) =>
          translateOptional(schema.getName) match {
            case Some(name) => (i, o + (name -> schema))
            case _          => (i, o)
          }
        case (accu, _) => accu
      }
    val translatedInputSchemas = translateSchemas(inputSchemas, schemaDefs)
    val translatedOutputSchemas = translateSchemas(outputSchemas, schemaDefs)
    translatedInputSchemas ++ translatedOutputSchemas
  }
}

case class CwlArray(itemType: CwlType,
                    id: Option[Identifier] = None,
                    label: Option[String] = None,
                    doc: Option[String] = None,
                    inputBinding: Option[CommandInputBinding] = None)
    extends CwlInputSchema {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlArray => itemType.coercibleTo(targetSchema.itemType)
      case _                      => false
    }
  }

  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CwlArray = {
    copy(
        id = id.map(_.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)),
        itemType = CwlType.copySimplifyIds(itemType,
                                           dropNamespace,
                                           replacePrefix,
                                           simplifyAutoNames,
                                           dropCwlExtension)
    )
  }

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[CwlArray]

  override def equals(that: Any): Boolean =
    that match {
      case a: CwlArray =>
        a match {
          case a if this eq a => true
          case a
              if a.canEqual(this)
                && (hashCode == a.hashCode)
                && (itemType == a.itemType)
                && (id == a.id || (hasRandomName() && a.hasRandomName())) =>
            true
          case _ => false
        }
      case _ =>
        false
    }

  override def hashCode(): Int =
    31 * (itemType.hashCode()) + {
      if (!hasRandomName()) {
        name.##
      } else 0
    }
}

object CwlArray {
  private def create(schema: ArraySchema,
                     cwlType: CwlType,
                     schemaDefs: Map[String, CwlSchema]): CwlArray = {
    val (name, label, doc) = schema match {
      case schema: InputArraySchema  => (schema.getName, schema.getLabel, schema.getDoc)
      case schema: OutputArraySchema => (schema.getName, schema.getLabel, schema.getDoc)
      case other                     => throw new RuntimeException(s"unexpected array schema ${other}")
    }
    val inputBinding = schema match {
      case c: CommandInputArraySchema =>
        translateOptional(c.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding.parse(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
      case _ => None
    }
    CwlArray(
        cwlType,
        translateOptional(name).map(Identifier.parse(_)),
        translateOptional(label),
        translateDoc(doc),
        inputBinding
    )
  }

  def apply(schema: ArraySchema, schemaDefs: Map[String, CwlSchema]): CwlArray = {
    val (cwlType, stdfile) = CwlType.translate(schema.getItems, schemaDefs)
    assert(stdfile.isEmpty)
    create(schema, cwlType, schemaDefs)
  }

  def translate(
      schema: ArraySchema,
      schemaDefs: Map[String, CwlSchema],
      rawSchemaDefs: Map[String, IOSchema]
  ): (CwlArray, Map[String, CwlSchema]) = {
    val (types, stdfile, newSchemaDefs) =
      CwlType.translateRaw(schema.getItems, schemaDefs, rawSchemaDefs)
    assert(stdfile.isEmpty)
    (create(schema, types, schemaDefs ++ newSchemaDefs), newSchemaDefs)
  }
}

sealed trait CwlRecordField extends Equals {
  val name: String
  val cwlType: CwlType
  val label: Option[String]
  val doc: Option[String]
  val secondaryFiles: Vector[SecondaryFile]
  val format: Vector[CwlValue]
  val streamable: Boolean

  /**
    * the field is optional if any of the allowed types are optional
    */
  lazy val optional: Boolean = CwlOptional.isOptional(cwlType)

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[CwlRecordField]

  override def equals(that: Any): Boolean =
    that match {
      case f: CwlRecordField =>
        f match {
          case f if this eq f => true
          case f
              if f.canEqual(this)
                && (hashCode == f.hashCode)
                && (cwlType == f.cwlType)
                && (name == f.name) =>
            true
          case _ => false
        }
      case _ =>
        false
    }

  override def hashCode(): Int =
    31 * (cwlType.hashCode()) + name.##
}

sealed trait CwlRecord extends CwlSchema {
  val fields: SeqMap[String, CwlRecordField]

  protected def fieldsCanBeCoercedTo(targetFields: SeqMap[String, CwlRecordField]): Boolean = {
    targetFields.forall {
      case (name, toField) if fields.contains(name) =>
        fields(name).cwlType.coercibleTo(toField.cwlType)
      case (_, toField) if CwlOptional.isOptional(toField.cwlType) => true
      case _                                                       => false
    }
  }

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[CwlRecord]

  override def equals(that: Any): Boolean =
    that match {
      case r: CwlRecord =>
        r match {
          case r if this eq r => true
          case r
              if r.canEqual(this)
                && (hashCode == r.hashCode)
                && (fields == r.fields)
                && (id == r.id || (hasRandomName() && r.hasRandomName())) =>
            true
          case _ => false
        }
      case _ =>
        false
    }

  override def hashCode(): Int =
    31 * (fields.map(_.hashCode()).sum) + {
      if (!hasRandomName()) {
        name.##
      } else 0
    }
}

case class CwlGenericRecordField(name: String,
                                 cwlType: CwlType,
                                 label: Option[String] = None,
                                 doc: Option[String] = None,
                                 secondaryFiles: Vector[SecondaryFile] = Vector.empty,
                                 format: Vector[CwlValue] = Vector.empty,
                                 streamable: Boolean = false)
    extends CwlRecordField {
  def copySimplifyIds(dropNamespace: Boolean,
                      replacePrefix: (Either[Boolean, String], Option[String]),
                      simplifyAutoNames: Boolean,
                      dropCwlExtension: Boolean): CwlGenericRecordField = {
    copy(cwlType = CwlType.copySimplifyIds(cwlType,
                                           dropNamespace,
                                           replacePrefix,
                                           simplifyAutoNames,
                                           dropCwlExtension)
    )
  }
}

case class CwlGenericRecord(fields: SeqMap[String, CwlGenericRecordField],
                            id: Option[Identifier] = None,
                            label: Option[String] = None,
                            doc: Option[String] = None)
    extends CwlRecord {
  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CwlGenericRecord = {
    copy(
        id = id.map(_.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)),
        fields = fields.map(f =>
          (f._1,
           f._2.copySimplifyIds(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension))
        )
    )
  }

  /**
    * Returns true if this type can be coerced to targetType,
    * which is a non-optional, non-equal, and non-Any type.
    */
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlGenericRecord => fieldsCanBeCoercedTo(targetSchema.fields)
      case targetSchema: CwlInputRecord   => fieldsCanBeCoercedTo(targetSchema.fields)
      case targetSchema: CwlOutputRecord  => fieldsCanBeCoercedTo(targetSchema.fields)
      case _                              => false
    }
  }
}

case class CwlInputRecordField(name: String,
                               cwlType: CwlType,
                               label: Option[String] = None,
                               doc: Option[String] = None,
                               inputBinding: Option[CommandInputBinding] = None,
                               secondaryFiles: Vector[SecondaryFile] = Vector.empty,
                               format: Vector[CwlValue] = Vector.empty,
                               streamable: Boolean = false,
                               loadContents: Boolean = false,
                               loadListing: LoadListing.LoadListing = LoadListing.No)
    extends CwlRecordField {
  def copySimplifyIds(dropNamespace: Boolean,
                      replacePrefix: (Either[Boolean, String], Option[String]),
                      simplifyAutoNames: Boolean,
                      dropCwlExtension: Boolean): CwlInputRecordField = {
    copy(cwlType = CwlType.copySimplifyIds(cwlType,
                                           dropNamespace,
                                           replacePrefix,
                                           simplifyAutoNames,
                                           dropCwlExtension)
    )
  }
}

object CwlInputRecordField {
  private def create(field: InputRecordField,
                     cwlType: CwlType,
                     schemaDefs: Map[String, CwlSchema]): CwlInputRecordField = {
    val id = Identifier.parse(field.getName)
    val inputBinding = field match {
      case c: CommandInputRecordField =>
        translateOptional(c.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding.parse(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
      case _ => None
    }
    CwlInputRecordField(
        id.name,
        cwlType,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
        inputBinding,
        translateOptionalArray(field.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs, isInput = true)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(field.getFormat).map(CwlValue.apply(_, schemaDefs)),
        translateOptional(field.getStreamable).exists(_.booleanValue()),
        translateOptional(field.getLoadContents).exists(_.booleanValue()),
        translateOptional(field.getLoadListing).map(LoadListing.from).getOrElse(LoadListing.No)
    )
  }

  def apply(field: InputRecordField, schemaDefs: Map[String, CwlSchema]): CwlInputRecordField = {
    val (cwlType, stdfile) = CwlType.translate(field.getType, schemaDefs)
    assert(stdfile.isEmpty)
    create(field, cwlType, schemaDefs)
  }

  def translate(
      field: InputRecordField,
      schemaDefs: Map[String, CwlSchema],
      rawSchemaDefs: Map[String, IOSchema]
  ): (CwlInputRecordField, Map[String, CwlSchema]) = {
    val (cwlType, stdfile, newSchemaDefs) =
      CwlType.translateRaw(field.getType, schemaDefs, rawSchemaDefs)
    assert(stdfile.isEmpty)
    (create(field, cwlType, schemaDefs ++ newSchemaDefs), newSchemaDefs)
  }
}

case class CwlInputRecord(fields: SeqMap[String, CwlInputRecordField],
                          id: Option[Identifier] = None,
                          label: Option[String] = None,
                          doc: Option[String] = None,
                          inputBinding: Option[CommandInputBinding] = None)
    extends CwlRecord
    with CwlInputSchema {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlGenericRecord => fieldsCanBeCoercedTo(targetSchema.fields)
      case targetSchema: CwlInputRecord   => fieldsCanBeCoercedTo(targetSchema.fields)
      case _                              => false
    }
  }

  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CwlInputRecord = {
    copy(
        id = id.map(_.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)),
        fields = fields.map(f =>
          (f._1,
           f._2.copySimplifyIds(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension))
        )
    )
  }
}

object CwlInputRecord {
  private def create(schema: InputRecordSchema,
                     fields: SeqMap[String, CwlInputRecordField],
                     schemaDefs: Map[String, CwlSchema]): CwlInputRecord = {
    val inputBinding = schema match {
      case c: CommandInputRecordSchema =>
        translateOptional(c.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding.parse(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
      case _ => None
    }
    CwlInputRecord(
        fields,
        translateOptional(schema.getName).map(Identifier.parse(_)),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc),
        inputBinding
    )
  }

  def apply(schema: InputRecordSchema, schemaDefs: Map[String, CwlSchema]): CwlInputRecord = {
    val fields = translateOptional(schema.getFields)
      .map(
          _.asScala
            .map {
              case field: CommandInputRecordFieldImpl =>
                val cwlField = CwlInputRecordField(field, schemaDefs)
                cwlField.name -> cwlField
              case other =>
                throw new RuntimeException(s"invalid record field ${other}")
            }
            .to(TreeSeqMap)
      )
      .getOrElse(SeqMap.empty[String, CwlInputRecordField])
    create(schema, fields, schemaDefs)
  }

  def translate(
      schema: InputRecordSchema,
      schemaDefs: Map[String, CwlSchema],
      rawSchemaDefs: Map[String, IOSchema]
  ): (CwlRecord, Map[String, CwlSchema]) = {
    val (fields, newSchemaDefs) = translateOptional(schema.getFields)
      .map { fields =>
        val (cwlFields, newSchemaDefs) =
          fields.asScala.foldLeft(Vector.empty[CwlInputRecordField], Map.empty[String, CwlSchema]) {
            case ((fieldAccu, schemaDefAccu), field: InputRecordField) =>
              val (cwlField, newSchemaDefs) =
                CwlInputRecordField.translate(field, schemaDefs ++ schemaDefAccu, rawSchemaDefs)
              (fieldAccu :+ cwlField, schemaDefAccu ++ newSchemaDefs)
            case other =>
              throw new RuntimeException(s"invalid record field ${other}")
          }
        (cwlFields.map(f => f.name -> f).to(TreeSeqMap), newSchemaDefs)
      }
      .getOrElse((SeqMap.empty[String, CwlInputRecordField], Map.empty[String, CwlSchema]))
    (create(schema, fields, schemaDefs), newSchemaDefs)
  }
}

case class CwlOutputRecordField(name: String,
                                cwlType: CwlType,
                                label: Option[String] = None,
                                doc: Option[String] = None,
                                outputBinding: Option[CommandOutputBinding] = None,
                                secondaryFiles: Vector[SecondaryFile] = Vector.empty,
                                format: Vector[CwlValue] = Vector.empty,
                                streamable: Boolean = false)
    extends CwlRecordField {
  def copySimplifyIds(dropNamespace: Boolean,
                      replacePrefix: (Either[Boolean, String], Option[String]),
                      simplifyAutoNames: Boolean,
                      dropCwlExtension: Boolean): CwlOutputRecordField = {
    copy(cwlType = CwlType.copySimplifyIds(cwlType,
                                           dropNamespace,
                                           replacePrefix,
                                           simplifyAutoNames,
                                           dropCwlExtension)
    )
  }
}

object CwlOutputRecordField {
  private def create(field: OutputRecordField,
                     cwlType: CwlType,
                     schemaDefs: Map[String, CwlSchema]): CwlOutputRecordField = {
    val id = Identifier.parse(field.getName)
    val outputBinding = field match {
      case c: CommandOutputRecordField =>
        translateOptional(c.getOutputBinding).map {
          case binding: CommandOutputBindingImpl => CommandOutputBinding.parse(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
      case _ => None
    }
    CwlOutputRecordField(
        id.name,
        cwlType,
        translateOptional(field.getLabel),
        translateDoc(field.getDoc),
        outputBinding,
        translateOptionalArray(field.getSecondaryFiles).map {
          case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs, isInput = false)
          case other =>
            throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
        },
        translateOptionalArray(field.getFormat).map(CwlValue.apply(_, schemaDefs)),
        translateOptional(field.getStreamable).exists(_.booleanValue())
    )
  }

  def apply(field: OutputRecordField, schemaDefs: Map[String, CwlSchema]): CwlOutputRecordField = {
    val (cwlType, stdfile) = CwlType.translate(field.getType, schemaDefs)
    assert(stdfile.isEmpty)
    create(field, cwlType, schemaDefs)
  }

  def translate(
      field: OutputRecordField,
      schemaDefs: Map[String, CwlSchema],
      rawSchemaDefs: Map[String, IOSchema]
  ): (CwlOutputRecordField, Map[String, CwlSchema]) = {
    val (types, stdfile, newSchemaDefs) =
      CwlType.translateRaw(field.getType, schemaDefs, rawSchemaDefs)
    assert(stdfile.isEmpty)
    (create(field, types, schemaDefs ++ newSchemaDefs), newSchemaDefs)
  }
}

case class CwlOutputRecord(fields: SeqMap[String, CwlOutputRecordField],
                           id: Option[Identifier] = None,
                           label: Option[String] = None,
                           doc: Option[String] = None)
    extends CwlRecord {
  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlGenericRecord => fieldsCanBeCoercedTo(targetSchema.fields)
      case targetSchema: CwlOutputRecord  => fieldsCanBeCoercedTo(targetSchema.fields)
      case _                              => false
    }
  }

  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CwlOutputRecord = {
    copy(
        id = id.map(_.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)),
        fields = fields.map(f =>
          (f._1,
           f._2.copySimplifyIds(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension))
        )
    )
  }
}

object CwlOutputRecord {
  private def create(schema: OutputRecordSchema,
                     fields: SeqMap[String, CwlOutputRecordField]): CwlOutputRecord = {
    CwlOutputRecord(
        fields,
        translateOptional(schema.getName).map(Identifier.parse(_)),
        translateOptional(schema.getLabel),
        translateDoc(schema.getDoc)
    )
  }

  def apply(schema: OutputRecordSchema, schemaDefs: Map[String, CwlSchema]): CwlOutputRecord = {
    val fields = translateOptional(schema.getFields)
      .map(
          _.asScala
            .map {
              case field: CommandOutputRecordFieldImpl =>
                val cwlField = CwlOutputRecordField(field, schemaDefs)
                cwlField.name -> cwlField
              case other =>
                throw new RuntimeException(s"invalid record field ${other}")
            }
            .to(TreeSeqMap)
      )
      .getOrElse(SeqMap.empty[String, CwlOutputRecordField])
    create(schema, fields)
  }

  def translate(
      schema: OutputRecordSchema,
      schemaDefs: Map[String, CwlSchema],
      rawSchemaDefs: Map[String, IOSchema]
  ): (CwlRecord, Map[String, CwlSchema]) = {
    val (fields, newSchemaDefs) = translateOptional(schema.getFields)
      .map { fields =>
        val (cwlFields, newSchemaDefs) =
          fields.asScala
            .foldLeft(Vector.empty[CwlOutputRecordField], Map.empty[String, CwlSchema]) {
              case ((fieldAccu, schemaDefAccu), field: OutputRecordField) =>
                val (cwlField, newSchemaDefs) =
                  CwlOutputRecordField.translate(field, schemaDefs ++ schemaDefAccu, rawSchemaDefs)
                (fieldAccu :+ cwlField, schemaDefAccu ++ newSchemaDefs)
              case other =>
                throw new RuntimeException(s"invalid record field ${other}")
            }
        (cwlFields.map(f => f.name -> f).to(TreeSeqMap), newSchemaDefs)
      }
      .getOrElse((SeqMap.empty[String, CwlOutputRecordField], Map.empty[String, CwlSchema]))
    (create(schema, fields), newSchemaDefs)
  }
}

case class CwlEnum(symbols: Vector[String],
                   id: Option[Identifier] = None,
                   label: Option[String] = None,
                   doc: Option[String] = None,
                   inputBinding: Option[CommandInputBinding] = None)
    extends CwlInputSchema {

  /**
    * symbols may be fully namespaced - this method returns just name part (after the last '/').
    */
  lazy val symbolNames: Vector[String] = {
    symbols.map {
      case CwlEnum.symbolRegex(_, name) => name
      case other                        => throw new Exception(s"invalid symbol ${other}")
    }
  }

  override protected def canBeCoercedTo(targetType: CwlType): Boolean = {
    targetType match {
      case targetSchema: CwlEnum if this.symbols == targetSchema.symbols => true
      case CwlString                                                     => true
      case _                                                             => false
    }
  }

  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): CwlEnum = {
    copy(
        id = id.map(_.simplify(dropNamespace, replacePrefix, simplifyAutoNames, dropCwlExtension)),
        symbols = symbols.map(s => {
          val frag = Identifier.parseUri(s)._2
          try {
            frag
              .map(Identifier.simplifyFrag(_, replacePrefix, simplifyAutoNames, dropCwlExtension))
              .get
          } catch {
            case ex: Throwable =>
              throw new Exception(s"error simplifying enum symbol ${s}", ex)
          }
        })
    )
  }

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[CwlEnum]

  override def equals(that: Any): Boolean =
    that match {
      case e: CwlEnum =>
        e match {
          case e if this eq e => true
          case e
              if e.canEqual(this)
                && (hashCode == e.hashCode)
                && (symbolNames == e.symbolNames)
                && (id == e.id || (hasRandomName() && e.hasRandomName())) =>
            true
          case _ => false
        }
      case _ =>
        false
    }

  override def hashCode(): Int =
    31 * (symbolNames.map(_.##).sum) + {
      if (!hasRandomName()) {
        name.##
      } else 0
    }
}

object CwlEnum {
  val symbolRegex: Regex = "(.+/)?(.+)".r

  def apply(schema: EnumSchema, schemaDefs: Map[String, CwlSchema]): CwlEnum = {
    val (name, label, doc) = schema match {
      case schema: InputEnumSchema  => (schema.getName, schema.getLabel, schema.getDoc)
      case schema: OutputEnumSchema => (schema.getName, schema.getLabel, schema.getDoc)
      case other                    => throw new RuntimeException(s"unexpected array schema ${other}")
    }
    val inputBinding = schema match {
      case c: CommandInputEnumSchema =>
        translateOptional(c.getInputBinding).map {
          case binding: CommandLineBindingImpl => CommandInputBinding.parse(binding, schemaDefs)
          case other =>
            throw new RuntimeException(s"unexpected CommandLineBinding value ${other}")
        }
      case _ => None
    }
    CwlEnum(
        schema.getSymbols.asScala.map {
          case s: String => s
          case other     => throw new Exception(s"unexpected symbol value ${other}")
        }.toVector,
        translateOptional(name).map(Identifier.parse(_)),
        translateOptional(label),
        translateDoc(doc),
        inputBinding
    )
  }
}
