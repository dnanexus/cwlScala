package dx.cwl

import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{DirectoryImpl, FileImpl}
import spray.json._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

/**
  * Marker trait for all CWL values.
  */
sealed trait CwlValue {

  /**
    * Converts this value to JSON
    */
  def toJson: JsValue

  /**
    * Returns true if this value is coercible to the specified type
    */
  def coercibleTo(targetType: CwlType): Boolean

  /**
    * Returns true if this value is coercible to any of the specified types
    */
  def coercibleTo(targetTypes: Vector[CwlType]): Boolean = {
    targetTypes.exists(coercibleTo)
  }
}

/**
  * Parent of all primitive types. Every primitive value maps
  * to a single, static [[CwlType]].
  */
sealed trait PrimitiveValue extends CwlValue {
  val cwlType: CwlType

  override def coercibleTo(targetType: CwlType): Boolean = {
    cwlType.coercibleTo(targetType)
  }
}

object CwlValue {

  /**
    * Translates a Map-type value to a schema type.
    * @param map the [[java.util.Map]] value
    * @param cwlType the target schema type
    * @param schemaDefs schema definitions to use when resolving the types of any members
    *                   of the schema type
    * @return a [[CwlValue]]
    */
  def applySchema(map: java.util.Map[_, _],
                  cwlType: CwlSchema,
                  schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlType match {
      case recordSchema: CwlRecord =>
        ObjectValue(map, recordSchema, schemaDefs)
      case arraySchema: CwlArray =>
        map.get("values") match {
          case array: java.util.Collection[_] =>
            ArrayValue(array, arraySchema, schemaDefs)
          case null =>
            throw new Exception(s"invalid array value ${map}")
        }
      case enumSchema: CwlEnum =>
        map.get("symbol") match {
          case null =>
            throw new Exception(s"invalid enum value ${map}")
          case symbol: String if enumSchema.symbols.contains(symbol) =>
            StringValue(symbol)
          case other =>
            throw new Exception(s"invalid enum symbol ${other}")
        }
    }
  }

  /**
    * Translates a Map-type value, which must at a minimum have a `class` key whose
    * value is `File`, `Directory`, or a previously defined schema type.
    * @param map the [[java.util.Map]] value
    * @param schemaDefs schema definitions to use when resolving the value's type
    * @return
    */
  def applyMap(map: java.util.Map[_, _], schemaDefs: Map[String, CwlSchema]): CwlValue = {
    map.get("class") match {
      case null        => ObjectValue(map, schemaDefs)
      case "File"      => FileValue(map)
      case "Directory" => DirectoryValue(map)
      case schemaName: String =>
        applySchema(map, schemaDefs(schemaName), schemaDefs)
      case other =>
        throw new Exception(s"unrecognized class ${other}")
    }
  }

  /**
    * Translates a Java value to a [[CwlValue]], absent of type information.
    * @param value the value to translate
    * @param schemaDefs any schema definitions to use when resolving Map-type
    *                   values that specify their `class`
    * @return a [[CwlValue]]
    */
  def apply(value: java.lang.Object, schemaDefs: Map[String, CwlSchema]): CwlValue = {
    value match {
      case null                       => NullValue
      case s: String                  => StringValue(s)
      case b: java.lang.Boolean       => BooleanValue(b.booleanValue())
      case i: java.lang.Integer       => IntValue(i.toInt)
      case l: java.lang.Long          => LongValue(l.toLong)
      case f: java.lang.Float         => FloatValue(f.toFloat)
      case d: java.lang.Double        => DoubleValue(d.toDouble)
      case file: FileImpl             => FileValue(file)
      case directory: DirectoryImpl   => DirectoryValue(directory)
      case a: java.util.Collection[_] => ArrayValue(a, schemaDefs)
      case m: java.util.Map[_, _]     => applyMap(m, schemaDefs)
      case bd: java.math.BigDecimal   => DoubleValue(bd.doubleValue())
      case bi: java.math.BigInteger =>
        try {
          LongValue(bi.longValueExact())
        } catch {
          case ex: ArithmeticException =>
            throw new RuntimeException(s"invalid long value ${bi.toString}", ex)
        }
      case _ =>
        throw new RuntimeException(s"cannot translate ${value} without a type specification")
    }
  }

  def applyMap(map: java.util.Map[_, _],
               cwlType: Option[CwlType] = None,
               schemaDefs: Map[String, CwlSchema] = Map.empty): CwlValue = {
    (cwlType, map.get("class")) match {
      case (Some(CwlFile), "File")           => FileValue(map)
      case (Some(CwlDirectory), "Directory") => DirectoryValue(map)
      case (None, null)                      => ObjectValue(map, schemaDefs)
      case (Some(schema: CwlSchema), null) =>
        applySchema(map, schema, schemaDefs)
      case (None, schemaName: String) if schemaDefs.contains(schemaName) =>
        applySchema(map, schemaDefs(schemaName), schemaDefs)
      case _ =>
        throw new Exception(s"cannot translate ${map} to value of type ${cwlType}")
    }
  }

  /**
    * Translates a Java value to a [[CwlValue]] of the specified type.
    *
    * Some limited auto-coersion is applied, e.g. {{{apply("1.0", CwlInt)}}}
    * is translated to {{{IntValue(1)}}}, but {{{apply("1.1", CwlInt)}}}
    * throws an exception because "1.1" cannot be converted exactly to an Int.
    * @param value the value to translate
    * @param cwlType the target type
    * @param schemaDefs any schema definitions to use when resolving Map-type
    *                   values that specify their `class`
    * @return a [[CwlValue]]
    */
  def apply(value: java.lang.Object,
            cwlType: CwlType,
            schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlType match {
      case CwlString               => StringValue(value)
      case CwlBoolean              => BooleanValue(value)
      case CwlInt                  => IntValue(value)
      case CwlLong                 => LongValue(value)
      case CwlFloat                => FloatValue(value)
      case CwlDouble               => DoubleValue(value)
      case CwlFile                 => FileValue(value)
      case CwlDirectory            => DirectoryValue(value)
      case arraySchema: CwlArray   => ArrayValue(value, arraySchema, schemaDefs)
      case recordSchema: CwlRecord => ObjectValue(value, recordSchema, schemaDefs)
      case enumSchema: CwlEnum     => StringValue.fromEnum(value, enumSchema)
      case CwlOptional(_) if value == null =>
        NullValue
      case CwlOptional(t) =>
        apply(value, t, schemaDefs)
      case CwlNull if value == null =>
        NullValue
      case CwlAny if value != null =>
        apply(value, schemaDefs)
      case _ =>
        throw new RuntimeException(s"cannot translate ${value} to value of type ${cwlType}")
    }
  }

  /**
    * Translates a Java value to a [[CwlValue]] of one of the specified types. Each type
    * is tried in order, and the result is the first one to return a value.
    * @param value the value to translate
    * @param cwlTypes the target types
    * @param schemaDefs any schema definitions to use when resolving Map-type
    *                   values that specify their `class`
    * @return a [[CwlValue]]
    */
  def apply(value: java.lang.Object,
            cwlTypes: Vector[CwlType],
            schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlTypes.iterator
      .map { t =>
        try {
          Some(apply(value, t, schemaDefs))
        } catch {
          case _: Throwable => None
        }
      }
      .collectFirst {
        case Some(value) => value
      }
      .getOrElse(
          throw new Exception(
              s"cannot translate ${value} to any of types ${cwlTypes.mkString(",")}"
          )
      )
  }

  /**
    * Serializes a [[Map[String, CwlValue]] to a [[JsObject]] using the
    * `toJson` method of the values.
    */
  def serializeMap(map: Map[String, CwlValue]): JsObject = {
    JsObject(map.view.mapValues(_.toJson).toMap)
  }

  /**
    * Deserializes a JSON value to a [[CwlValue]], absent type information.
    */
  def deserialize(jsValue: JsValue, schemaDefs: Map[String, CwlSchema]): CwlValue = {
    jsValue match {
      case JsNull       => NullValue
      case JsString(s)  => StringValue(s)
      case JsBoolean(b) => BooleanValue(b)
      case JsNumber(n) =>
        try {
          LongValue(n.toLongExact)
        } catch {
          case _: ArithmeticException =>
            DoubleValue(n.toDouble)
        }
      case JsArray(a) =>
        ArrayValue(a.map(deserialize(_, schemaDefs)))
      case JsObject(o) =>
        o.get("class") match {
          case Some(JsString(schemaName)) if schemaDefs.contains(schemaName) =>
            schemaDefs(schemaName) match {
              case schema: CwlArray  => ArrayValue.apply(jsValue, schema, schemaDefs)
              case schema: CwlRecord => ObjectValue.apply(jsValue, schema, schemaDefs)
              case schema: CwlEnum   => StringValue.apply(jsValue, Some(schema))
            }
          case _ =>
            ObjectValue(o.view.mapValues(deserialize(_, schemaDefs)).toMap)
        }
      case _ =>
        throw new Exception(s"cannot deserialize ${jsValue} without type information")
    }
  }

  /**
    * Deserializes a JSON value to a [[CwlValue]] given type information.
    * @param jsValue the JSON value
    * @param cwlType the target type
    * @param schemaDefs schema defintions to use when resolving types
    * @return a [[CwlValue]]
    */
  @tailrec
  def deserialize(jsValue: JsValue,
                  cwlType: CwlType,
                  schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlType match {
      case CwlString         => StringValue.apply(jsValue)
      case CwlBoolean        => BooleanValue.apply(jsValue)
      case CwlInt            => IntValue.apply(jsValue)
      case CwlLong           => LongValue.apply(jsValue)
      case CwlFloat          => FloatValue.apply(jsValue)
      case CwlDouble         => DoubleValue.apply(jsValue)
      case CwlFile           => FileValue.apply(jsValue)
      case CwlDirectory      => DirectoryValue.apply(jsValue)
      case schema: CwlArray  => ArrayValue(jsValue, schema, schemaDefs)
      case schema: CwlRecord => ObjectValue(jsValue, schema, schemaDefs)
      case schema: CwlEnum   => StringValue(jsValue, Some(schema))
      case CwlOptional(_) if jsValue == JsNull =>
        NullValue
      case CwlOptional(t) =>
        deserialize(jsValue, t, schemaDefs)
      case CwlNull if jsValue == JsNull =>
        NullValue
      case CwlAny if jsValue != JsNull =>
        deserialize(jsValue, schemaDefs)
      case _ =>
        throw new Exception(s"cannot deserialize ${jsValue} as type ${cwlType}")
    }
  }

  /**
    * Deserializes a JSON value to a [[CwlValue]] given type information. Each type
    * * is tried in order, and the result is the first one to return a value.
    * @param jsValue the JSON value
    * @param cwlTypes the target types
    * @param schemaDefs schema defintions to use when resolving types
    * @return tuple ([[CwlType]], [[CwlValue]])
    */
  def deserialize(jsValue: JsValue,
                  cwlTypes: Vector[CwlType],
                  schemaDefs: Map[String, CwlSchema]): (CwlType, CwlValue) = {
    cwlTypes.iterator
      .map { t =>
        try {
          Some((t, deserialize(jsValue, t, schemaDefs)))
        } catch {
          case _: Throwable => None
        }
      }
      .collectFirst {
        case Some(result) => result
      }
      .getOrElse(
          throw new Exception(
              s"cannot deserialize ${jsValue} to any of types ${cwlTypes.mkString(",")}"
          )
      )
  }
}

/**
  * Trait of [[CwlType]]s that allow indexing with a string key.
  */
sealed trait StringIndexable {
  def contains(key: String): Boolean

  def get(key: String): Option[CwlValue]

  def apply(key: String): CwlValue = {
    get(key).getOrElse(NullValue)
  }
}

/**
  * Trait of [[CwlType]]s that allow indexing with an integer key.
  */
sealed trait IntIndexable {
  def length: Int

  def get(index: Int): Option[CwlValue]

  def apply(index: Int): CwlValue = {
    get(index).getOrElse(NullValue)
  }
}

case object NullValue extends PrimitiveValue {
  override val cwlType: CwlType = CwlNull

  override val toJson: JsValue = JsNull
}

case class StringValue(value: String) extends PrimitiveValue with IntIndexable {
  override val cwlType: CwlType = CwlString

  override def get(index: Int): Option[CwlValue] = {
    Some(StringValue(value.charAt(index).toString))
  }

  override lazy val toJson: JsValue = JsString(value)

  override def length: Int = value.length
}

object StringValue {
  lazy val empty: StringValue = StringValue("")
  lazy val opaque: StringValue = StringValue(OpaqueValue)

  def isOpaque(value: StringValue): Boolean = {
    value.value == OpaqueValue
  }

  def apply(obj: Any): StringValue = {
    obj match {
      case s: String => StringValue(s)
      case _         => StringValue(obj.toString)
    }
  }

  def fromEnum(obj: Any, enum: CwlEnum): StringValue = {
    obj match {
      case symbol: String if enum.symbols.contains(symbol) => StringValue(symbol)
      case _ =>
        throw new Exception(s"symbol ${obj} is not one of ${enum.symbols.mkString(",")}")
    }
  }

  def apply(jsValue: JsValue, schema: Option[CwlEnum] = None): StringValue = {
    (schema, jsValue) match {
      case (None, JsString(s)) => StringValue(s)
      case (None, _)           => StringValue(jsValue.prettyPrint)
      case (Some(enum), JsString(symbol)) if enum.symbols.contains(symbol) =>
        StringValue(symbol)
      case (Some(_), JsObject(fields)) if fields.contains("symbol") =>
        apply(fields("symbol"), schema)
      case _ =>
        throw new Exception(s"invalid ${schema} value ${jsValue}")
    }
  }
}

case class BooleanValue(value: Boolean) extends PrimitiveValue {
  override val cwlType: CwlType = CwlBoolean

  override lazy val toJson: JsValue = JsBoolean(value)
}

object BooleanValue {
  lazy val True: BooleanValue = BooleanValue(true)
  lazy val False: BooleanValue = BooleanValue(false)

  def apply(obj: Any): BooleanValue = {
    obj match {
      case b: java.lang.Boolean => BooleanValue(b.booleanValue())
      case "true"               => BooleanValue(true)
      case "false"              => BooleanValue(false)
      case _ =>
        throw new Exception(s"invalid boolean value ${obj}")
    }
  }

  def apply(jsValue: JsValue): BooleanValue = {
    jsValue match {
      case JsBoolean(b)      => BooleanValue(b.booleanValue())
      case JsString("true")  => BooleanValue(true)
      case JsString("false") => BooleanValue(false)
      case _ =>
        throw new Exception(s"invalid boolean value ${jsValue}")
    }
  }
}

sealed trait NumericValue extends PrimitiveValue {

  /**
    * Gets the numeric value as a BigDecimal.
    */
  def decimalValue: BigDecimal
}

case class IntValue(value: Int) extends NumericValue {
  override val cwlType: CwlType = CwlInt

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)
}

object IntValue {
  def apply(obj: Any): IntValue = {
    try {
      obj match {
        case l: Long if l.isValidInt   => IntValue(l.toInt)
        case f: Float if f.isValidInt  => IntValue(f.toInt)
        case d: Double if d.isValidInt => IntValue(d.toInt)
        case i: java.lang.Integer      => IntValue(i.toInt)
        case l: java.lang.Long =>
          IntValue(java.math.BigInteger.valueOf(l).intValueExact())
        case f: java.lang.Float =>
          IntValue(java.math.BigDecimal.valueOf(f.toDouble).intValueExact())
        case d: java.lang.Double =>
          IntValue(java.math.BigDecimal.valueOf(d).intValueExact())
        case bi: java.math.BigInteger => IntValue(bi.intValueExact())
        case bd: java.math.BigDecimal => IntValue(bd.intValueExact())
        case s: String                => IntValue(s.toInt)
        case _ =>
          throw new Exception(s"invalid int value ${obj}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${obj} to int", ex)
    }
  }

  def apply(jsValue: JsValue): IntValue = {
    try {
      jsValue match {
        case JsNumber(n) => IntValue(n.intValue)
        case JsString(s) => IntValue(s.toInt)
        case _ =>
          throw new Exception(s"invalid int value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${jsValue} to int", ex)
    }
  }
}

case class LongValue(value: Long) extends NumericValue {
  override val cwlType: CwlType = CwlLong

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)
}

object LongValue {
  def apply(obj: Any): LongValue = {
    try {
      obj match {
        case i: Int => LongValue(i.longValue())
        case f: Float =>
          LongValue(java.math.BigDecimal.valueOf(f.toDouble).longValueExact())
        case d: Double =>
          LongValue(java.math.BigDecimal.valueOf(d).longValueExact())
        case f: java.lang.Float =>
          LongValue(java.math.BigDecimal.valueOf(f.toDouble).longValueExact())
        case d: java.lang.Double =>
          LongValue(java.math.BigDecimal.valueOf(d).longValueExact())
        case bi: java.math.BigInteger => LongValue(bi.longValueExact())
        case bd: java.math.BigDecimal => LongValue(bd.longValueExact())
        case n: java.lang.Number      => LongValue(n.longValue())
        case s: String                => LongValue(s.toLong)
        case _ =>
          throw new Exception(s"invalid long value ${obj}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${obj} to long", ex)
    }
  }

  def apply(jsValue: JsValue): LongValue = {
    try {
      jsValue match {
        case JsNumber(n) => LongValue(n.longValue)
        case JsString(s) => LongValue(s.toLong)
        case _ =>
          throw new Exception(s"invalid long value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${jsValue} to long", ex)
    }
  }
}

case class FloatValue(value: Float) extends NumericValue {
  override val cwlType: CwlType = CwlFloat

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)
}

object FloatValue {
  def apply(obj: Any): FloatValue = {
    try {
      obj match {
        case i: Int              => FloatValue(i.floatValue())
        case l: Long             => FloatValue(l.floatValue())
        case d: Double           => FloatValue(d.floatValue())
        case n: java.lang.Number => FloatValue(n.floatValue())
        case s: String           => FloatValue(s.toFloat)
        case _ =>
          throw new Exception(s"invalid float value ${obj}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${obj} to float", ex)
    }
  }

  def apply(jsValue: JsValue): FloatValue = {
    try {
      jsValue match {
        case JsNumber(n) => FloatValue(n.floatValue)
        case JsString(s) => FloatValue(s.toFloat)
        case _ =>
          throw new Exception(s"invalid float value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${jsValue} to float", ex)
    }
  }
}

case class DoubleValue(value: Double) extends NumericValue {
  override val cwlType: CwlType = CwlDouble

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)
}

object DoubleValue {
  def apply(obj: Any): DoubleValue = {
    try {
      obj match {
        case i: Int              => DoubleValue(i.doubleValue())
        case l: Long             => DoubleValue(l.doubleValue())
        case f: Float            => DoubleValue(f.doubleValue())
        case n: java.lang.Number => DoubleValue(n.doubleValue())
        case s: String           => DoubleValue(s.toDouble)
        case _ =>
          throw new Exception(s"invalid double value ${obj}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${obj} to double", ex)
    }
  }

  def apply(jsValue: JsValue): DoubleValue = {
    try {
      jsValue match {
        case JsNumber(n) => DoubleValue(n.doubleValue)
        case JsString(s) => DoubleValue(s.toDouble)
        case _ =>
          throw new Exception(s"invalid double value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new RuntimeException(s"cannot convert ${jsValue} to double", ex)
    }
  }
}

/**
  * Parent of the path-type values.
  */
sealed trait PathValue extends PrimitiveValue with StringIndexable {
  val location: Option[String]
  val path: Option[String]
  val basename: Option[String]

  def productElementNames: Iterator[String]

  override lazy val toJson: JsValue = {
    JsObject(productElementNames.flatMap { key =>
      get(key) match {
        case None        => None
        case Some(value) => Some(key -> value.toJson)
      }
    }.toMap)
  }
}

object PathValue {
  def apply(expr: java.lang.Object): PathValue = {
    expr match {
      case file: FileImpl           => apply(file)
      case directory: DirectoryImpl => apply(directory)
      case _ =>
        throw new RuntimeException(s"unexpected file/directory value ${expr}")
    }
  }

  def apply(map: java.util.Map[_, _]): PathValue = {
    map.get("class") match {
      case "File"      => FileValue(map)
      case "Directory" => DirectoryValue(map)
      case null =>
        throw new RuntimeException(s"missing required key 'class' in expression ${map}")
      case _ => throw new Exception(s"invalid path map ${map}")
    }
  }

  def apply(jsValue: JsValue): PathValue = {
    jsValue match {
      case JsObject(fields) if fields.contains("class") =>
        fields("class") match {
          case JsString("File")      => FileValue(fields)
          case JsString("Directory") => DirectoryValue(fields)
          case _ =>
            throw new Exception(s"invalid path object ${jsValue}")
        }
      case JsString(path) => FileValue(location = Some(path))
      case _ =>
        throw new Exception(s"invalid path value ${jsValue}")
    }
  }
}

case class FileValue(location: Option[String] = None,
                     path: Option[String] = None,
                     basename: Option[String] = None,
                     dirname: Option[String] = None,
                     nameroot: Option[String] = None,
                     nameext: Option[String] = None,
                     checksum: Option[String] = None,
                     size: Option[Long] = None,
                     secondaryFiles: Vector[PathValue] = Vector.empty,
                     format: Option[String] = None,
                     contents: Option[String] = None)
    extends PathValue {
  override val cwlType: CwlType = CwlFile
  private val keys: Set[String] = Set("location",
                                      "path",
                                      "basename",
                                      "dirname",
                                      "nameroot",
                                      "nameext",
                                      "checksum",
                                      "size",
                                      "secondaryFiles",
                                      "format",
                                      "contents")

  override def contains(key: String): Boolean = {
    keys.contains(key)
  }

  override def get(key: String): Option[CwlValue] = {
    key match {
      case "location"       => location.map(StringValue(_))
      case "path"           => path.map(StringValue(_))
      case "basename"       => basename.map(StringValue(_))
      case "dirname"        => dirname.map(StringValue(_))
      case "nameroot"       => nameroot.map(StringValue(_))
      case "nameext"        => nameext.map(StringValue(_))
      case "checksum"       => checksum.map(StringValue(_))
      case "size"           => size.map(LongValue(_))
      case "secondaryFiles" => Some(ArrayValue(secondaryFiles))
      case "format"         => format.map(StringValue(_))
      case "contents"       => contents.map(StringValue(_))
      case _ =>
        throw new Exception(s"invalid File property ${key}")
    }
  }
}

object FileValue {
  lazy val empty: FileValue = FileValue()

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
        translateOptionalArray(file.getSecondaryFiles).map(PathValue.apply),
        translateOptional(file.getFormat),
        translateOptional(file.getContents)
    )
  }

  def apply(map: java.util.Map[_, _]): FileValue = {
    FileValue(
        translateOptionalString(map.get("location")),
        translateOptionalString(map.get("path")),
        translateOptionalString(map.get("basename")),
        translateOptionalString(map.get("dirname")),
        translateOptionalString(map.get("nameroot")),
        translateOptionalString(map.get("nameext")),
        translateOptionalString(map.get("checksum")),
        translateOptionalString(map.get("size")).map(_.toLong),
        map.get("secondaryFiles") match {
          case null => Vector.empty
          case pathList: java.util.List[_] =>
            pathList.asScala.map {
              case paths: java.util.Map[_, _] => PathValue.apply(paths)
              case other =>
                throw new RuntimeException(s"unexpected path value ${other}")
            }.toVector
          case other =>
            throw new RuntimeException(s"unexpected secondaryFiles value ${other}")
        },
        translateOptionalString(map.get("format")),
        translateOptionalString(map.get("contents"))
    )
  }

  def apply(obj: Any): FileValue = {
    obj match {
      case s: String                => apply(location = Some(s))
      case file: FileImpl           => FileValue(file)
      case map: java.util.Map[_, _] => apply(map)
      case _ =>
        throw new Exception(s"invalid value value ${obj}")
    }
  }

  def apply(jsValue: JsValue): FileValue = {
    def unwrapString(jsValue: JsValue): String = {
      jsValue match {
        case JsString(s) => s
        case _ =>
          throw new Exception(s"expected string, not ${jsValue}")
      }
    }

    def unwrapLong(jsValue: JsValue): Long = {
      jsValue match {
        case JsNumber(n) => n.toLongExact
        case JsString(s) => s.toLong
        case _ =>
          throw new Exception(s"expected string, not ${jsValue}")
      }
    }

    jsValue match {
      case JsString(uri) => FileValue(location = Some(uri))
      case JsObject(fields) =>
        FileValue(
            fields.get("location").map(unwrapString),
            fields.get("path").map(unwrapString),
            fields.get("basename").map(unwrapString),
            fields.get("dirname").map(unwrapString),
            fields.get("nameroot").map(unwrapString),
            fields.get("nameext").map(unwrapString),
            fields.get("checksum").map(unwrapString),
            fields.get("size").map(unwrapLong),
            fields.get("secondaryFiles") match {
              case Some(JsArray(array)) => array.map(PathValue.apply)
              case None                 => Vector.empty
              case other =>
                throw new Exception(s"invalid secondaryFiles value ${other}")
            },
            fields.get("format").map(unwrapString),
            fields.get("contents").map(unwrapString)
        )
      case _ =>
        throw new Exception(s"invalid file value ${jsValue}")
    }
  }
}

case class DirectoryValue(location: Option[String] = None,
                          path: Option[String] = None,
                          basename: Option[String] = None,
                          listing: Vector[PathValue] = Vector.empty)
    extends PathValue {
  override val cwlType: CwlType = CwlDirectory
  private val keys: Set[String] = Set("location", "path", "basename", "listing")

  override def contains(key: String): Boolean = {
    keys.contains(key)
  }

  override def get(key: String): Option[CwlValue] = {
    key match {
      case "location" => location.map(StringValue(_))
      case "path"     => path.map(StringValue(_))
      case "basename" => basename.map(StringValue(_))
      case "listing"  => Some(ArrayValue(listing))
      case _ =>
        throw new Exception(s"invalid File property ${key}")
    }
  }
}

object DirectoryValue {
  def apply(directory: DirectoryImpl): DirectoryValue = {
    DirectoryValue(
        translateOptional(directory.getLocation),
        translateOptional(directory.getPath),
        translateOptional(directory.getBasename),
        translateOptionalArray(directory.getListing).map(PathValue.apply)
    )
  }

  def apply(map: java.util.Map[_, _]): DirectoryValue = {
    DirectoryValue(
        translateOptionalString(map.get("location")),
        translateOptionalString(map.get("path")),
        translateOptionalString(map.get("basename")),
        map.get("listing") match {
          case null => Vector.empty
          case pathList: java.util.List[_] =>
            pathList.asScala.map {
              case paths: java.util.Map[_, _] => PathValue.apply(paths)
              case other =>
                throw new RuntimeException(s"unexpected path value ${other}")
            }.toVector
          case other =>
            throw new RuntimeException(s"unexpected secondaryFiles value ${other}")
        }
    )
  }

  def apply(obj: Any): DirectoryValue = {
    obj match {
      case s: String                => apply(location = Some(s))
      case directory: DirectoryImpl => apply(directory)
      case map: java.util.Map[_, _] => apply(map)
      case _ =>
        throw new Exception(s"invalid directory value ${obj}")
    }
  }
}

/**
  * Indicates that a random filename should be generated to store the
  * contents of stdout/stderr.
  */
case class RandomFile(stdfile: StdFile.StdFile) extends PrimitiveValue {
  override val cwlType: CwlType = CwlFile

  override def toJson: JsValue = {
    JsObject("location" -> JsString(stdfile.toString))
  }
}

case class ArrayValue(items: Vector[CwlValue]) extends CwlValue with IntIndexable {
  override def length: Int = {
    items.size
  }

  override def get(index: Int): Option[CwlValue] = {
    Some(items(index))
  }

  override def toJson: JsValue = {
    JsArray(items.map(_.toJson))
  }

  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case schema: CwlArray => items.forall(_.coercibleTo(schema.itemTypes))
      case _                => false
    }
  }
}

object ArrayValue {
  lazy val empty: ArrayValue = ArrayValue(Vector())

  def apply(array: java.util.Collection[_], schemaDefs: Map[String, CwlSchema]): ArrayValue = {
    ArrayValue(
        array.asScala.toVector.map(obj => CwlValue(obj.asInstanceOf[java.lang.Object], schemaDefs))
    )
  }

  def apply(array: java.util.Collection[_],
            cwlType: CwlArray,
            schemaDefs: Map[String, CwlSchema]): ArrayValue = {
    ArrayValue(
        array.asScala.toVector.map(obj =>
          CwlValue(obj.asInstanceOf[java.lang.Object], cwlType.itemTypes, schemaDefs)
        )
    )
  }

  def apply(obj: Any, cwlType: CwlArray, schemaDefs: Map[String, CwlSchema]): ArrayValue = {
    obj match {
      case array: java.util.Collection[_] =>
        apply(array, cwlType, schemaDefs)
      case _ =>
        throw new Exception(s"invalid array value ${obj}")
    }
  }

  def apply(jsValue: JsValue, schema: CwlArray, schemaDefs: Map[String, CwlSchema]): ArrayValue = {
    jsValue match {
      case JsArray(array) =>
        ArrayValue(array.map(CwlValue(_, schema.itemTypes, schemaDefs)))
      case JsObject(fields) if fields.contains("values") =>
        fields("values") match {
          case array: JsArray => apply(array, schema, schemaDefs)
          case _ =>
            throw new Exception(s"invalid array value ${jsValue}")
        }
      case _ =>
        throw new Exception(s"invalid array value ${jsValue}")
    }
  }
}

trait ObjectLike extends CwlValue with StringIndexable

case class ObjectValue(members: Map[String, CwlValue]) extends ObjectLike {

  override def contains(key: String): Boolean = {
    members.contains(key)
  }

  override def get(key: String): Option[CwlValue] = {
    members.get(key)
  }

  override def toJson: JsValue = {
    CwlValue.serializeMap(members)
  }

  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case CwlAny => true
      case schema: CwlRecord =>
        schema.fields.values.forall { field =>
          if (members.contains(field.name)) {
            members(field.name).coercibleTo(field.types)
          } else if (field.optional) {
            true
          } else {
            false
          }
        }
      case _ => false
    }
  }
}

object ObjectValue {
  lazy val empty: ObjectValue = ObjectValue(Map.empty)

  def apply(map: java.util.Map[_, _], schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    ObjectValue(map.asScala.map {
      case (key: String, value) =>
        key -> CwlValue(value.asInstanceOf[java.lang.Object], schemaDefs)
      case other =>
        throw new Exception(s"invalid object member ${other}")
    }.toMap)
  }

  def apply(map: java.util.Map[_, _],
            schema: CwlRecord,
            schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    ObjectValue(schema.fields.view.mapValues { field =>
      map.get(field.name) match {
        case null if field.optional => NullValue
        case null =>
          throw new Exception(s"missing required field ${field.name} in ${map}")
        case obj: java.lang.Object =>
          CwlValue(obj, field.types, schemaDefs)
      }
    }.toMap)
  }

  def apply(obj: Any, cwlType: CwlRecord, schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    obj match {
      case map: java.util.Map[_, _] =>
        apply(map, cwlType, schemaDefs)
      case _ =>
        throw new Exception(s"invalid record value ${obj}")
    }
  }

  def apply(jsValue: JsValue,
            schema: CwlRecord,
            schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    val fields = jsValue.asJsObject.fields
    ObjectValue(schema.fields.view.mapValues { field =>
      if (fields.contains(field.name)) {
        CwlValue(fields(field.name), field.types, schemaDefs)
      } else if (field.optional) {
        NullValue
      } else {
        throw new Exception(s"missing required field ${field.name}")
      }
    }.toMap)
  }
}
