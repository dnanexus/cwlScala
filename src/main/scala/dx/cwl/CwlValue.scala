package dx.cwl

import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{DirectoryImpl, FileImpl}
import spray.json._

import scala.annotation.tailrec
import scala.collection.immutable.{SeqMap, TreeSeqMap}
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
  def coercibleTo(targetType: CwlType): Boolean = false

  def coerceTo(targetType: CwlType): CwlValue = ???
}

case object NullValue extends CwlValue {
  override val toJson: JsValue = JsNull

  override val toString: String = "null"

  /**
    * Returns true if this value is coercible to the specified type
    */
  override def coercibleTo(targetType: CwlType): Boolean = {
    CwlNull.coercibleTo(targetType)
  }

  override def coerceTo(targetType: CwlType): CwlValue = {
    if (targetType == CwlNull || CwlOptional.isOptional(targetType)) {
      NullValue
    } else {
      throw new Exception(s"null is not coercible to ${targetType}")
    }
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

  protected def coerceToOther(targetType: CwlType): CwlValue = ???

  override def coerceTo(targetType: CwlType): CwlValue = {
    targetType match {
      case this.cwlType | CwlAny => this
      case CwlOptional(t)        => coerceTo(t)
      case CwlMulti(types) =>
        types.iterator
          .map { t =>
            try {
              Some(coerceTo(t))
            } catch {
              case _: Throwable => None
            }
          }
          .collectFirst {
            case Some(result) => result
          }
          .getOrElse(
              throw new Exception(
                  s"cannot coerce ${this} to any of types ${types.mkString(",")}"
              )
          )
      case _ if coercibleTo(targetType) => coerceToOther(targetType)
      case _ =>
        throw new Exception(s"${this} is not coercible to ${targetType}")
    }
  }
}

object CwlValueContext extends Enumeration {
  type CwlValueContext = Value
  val Input, Output = Value
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
  def apply(value: Any, schemaDefs: Map[String, CwlSchema]): CwlValue = {
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
            throw new Exception(s"invalid long value ${bi.toString}", ex)
        }
      case _ =>
        throw new Exception(s"cannot translate ${value} without a type specification")
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
        throw new Exception(s"cannot translate ${value} to value of type ${cwlType}")
    }
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
              case schema: CwlArray  => ArrayValue.deserialize(jsValue, schema, schemaDefs)
              case schema: CwlRecord => ObjectValue.deserialize(jsValue, schema, schemaDefs)
              case schema: CwlEnum   => StringValue.deserialize(jsValue, schema)
            }
          case _ =>
            ObjectValue(
                o.map {
                    case (name, value) => name -> deserialize(value, schemaDefs)
                  }
                  .to(TreeSeqMap)
            )
        }
      case _ =>
        throw new Exception(s"cannot deserialize ${jsValue} without type information")
    }
  }

  @tailrec
  private def deserializeSingle(jsValue: JsValue,
                                cwlType: CwlType,
                                schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlType match {
      case CwlString         => StringValue.deserialize(jsValue)
      case CwlBoolean        => BooleanValue.deserialize(jsValue)
      case CwlInt            => IntValue.deserialize(jsValue)
      case CwlLong           => LongValue.deserialize(jsValue)
      case CwlFloat          => FloatValue.deserialize(jsValue)
      case CwlDouble         => DoubleValue.deserialize(jsValue)
      case CwlFile           => FileValue.deserialize(jsValue)
      case CwlDirectory      => DirectoryValue.deserialize(jsValue)
      case schema: CwlArray  => ArrayValue.deserialize(jsValue, schema, schemaDefs)
      case schema: CwlRecord => ObjectValue.deserialize(jsValue, schema, schemaDefs)
      case schema: CwlEnum   => StringValue.deserialize(jsValue, schema)
      case CwlOptional(_) if jsValue == JsNull =>
        NullValue
      case CwlOptional(t) =>
        deserializeSingle(jsValue, t, schemaDefs)
      case CwlNull if jsValue == JsNull =>
        NullValue
      case CwlAny if jsValue != JsNull =>
        deserialize(jsValue, schemaDefs)
      case _ =>
        throw new Exception(s"cannot deserialize ${jsValue} as type ${cwlType}")
    }
  }

  private def deserializeMulti(jsValue: JsValue,
                               cwlType: CwlMulti,
                               schemaDefs: Map[String, CwlSchema]): (CwlType, CwlValue) = {
    cwlType.types.iterator
      .map {
        case CwlAny => None
        case t =>
          try {
            Some((t, deserialize(jsValue, t, schemaDefs)._2))
          } catch {
            case _: Throwable => None
          }
      }
      .collectFirst {
        case Some(result) => result
      }
      .getOrElse(
          if (cwlType.types.contains(CwlAny)) {
            (CwlAny, deserialize(jsValue, schemaDefs))
          } else {
            throw new Exception(
                s"cannot deserialize ${jsValue} to any of types ${cwlType.types.mkString(",")}"
            )
          }
      )
  }

  /**
    * Deserializes a JSON value to a [[CwlValue]] given type information.
    * @param jsValue the JSON value
    * @param cwlType the target type
    * @param schemaDefs schema defintions to use when resolving types
    * @return ([[CwlType]], [[CwlValue]]), where the CwlType is the actual
    *         type of the return value
    */
  def deserialize(jsValue: JsValue,
                  cwlType: CwlType,
                  schemaDefs: Map[String, CwlSchema]): (CwlType, CwlValue) = {
    (cwlType, jsValue) match {
      case (CwlOptional(_: CwlMulti), JsNull) => (cwlType, NullValue)
      case (m: CwlMulti, _)                   => deserializeMulti(jsValue, m, schemaDefs)
      case _                                  => (cwlType, deserializeSingle(jsValue, cwlType, schemaDefs))
    }
  }

  def deserializeMap(map: Map[String, JsValue],
                     schemaDefs: Map[String, CwlSchema] = Map.empty): Map[String, CwlValue] = {
    map.map {
      case (name, jsValue) => name -> deserialize(jsValue, schemaDefs)
    }
  }

  /**
    * Infers the CwlType for the given value.
    */
  def inferType(value: CwlValue, ctx: CwlValueContext.CwlValueContext): CwlType = {
    value match {
      case NullValue         => CwlNull
      case _: BooleanValue   => CwlBoolean
      case _: IntValue       => CwlInt
      case _: LongValue      => CwlLong
      case _: FloatValue     => CwlFloat
      case _: DoubleValue    => CwlDouble
      case _: StringValue    => CwlString
      case _: FileValue      => CwlFile
      case _: RandomFile     => CwlFile
      case _: DirectoryValue => CwlDirectory
      case ArrayValue(array) =>
        val itemType = array.map(inferType(_, ctx)).distinct match {
          case Vector(t) => t
          case types     => CwlType.flatten(types)
        }
        CwlArray(itemType)
      case schema: ObjectLike if ctx == CwlValueContext.Input =>
        CwlInputRecord(schema.fields.map {
          case (name, value) =>
            val cwlType = inferType(value, ctx)
            name -> CwlInputRecordField(name, cwlType)
        })
      case schema: ObjectLike if ctx == CwlValueContext.Output =>
        CwlOutputRecord(schema.fields.map {
          case (name, value) =>
            val cwlType = inferType(value, ctx)
            name -> CwlOutputRecordField(name, cwlType)
        })
      case _ => throw new Exception(s"unexpected value ${value}")
    }
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

case class StringValue(value: String) extends PrimitiveValue with IntIndexable {
  override val cwlType: CwlType = CwlString

  override def get(index: Int): Option[CwlValue] = {
    Some(StringValue(value.charAt(index).toString))
  }

  override lazy val toJson: JsValue = JsString(value)

  override def length: Int = value.length

  override def toString: String = value

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case CwlBoolean => BooleanValue(value.trim.toBoolean)
      case CwlInt     => IntValue(value.trim.toInt)
      case CwlLong    => LongValue(value.trim.toLong)
      case CwlFloat   => FloatValue(value.trim.toFloat)
      case CwlDouble  => DoubleValue(value.trim.toDouble)
      // TODO: not sure whether to trim files/directories, since they may be allowed to have whitespace
      case CwlFile      => FileValue(value)
      case CwlDirectory => DirectoryValue(value)
      case enum: CwlEnum if enum.symbols.contains(value) || enum.symbolNames.contains(value) =>
        this
      case enum: CwlEnum
          if enum.symbols.contains(value.trim) || enum.symbolNames.contains(value.trim) =>
        StringValue(value.trim)
      case _ => throw new RuntimeException
    }
  }
}

object StringValue {
  lazy val empty: StringValue = StringValue("")
  lazy val opaque: StringValue = StringValue(OpaqueValue)

  def isOpaque(s: StringValue): Boolean = {
    s.value == OpaqueValue
  }

  def apply(obj: Any): StringValue = {
    obj match {
      case s: String => StringValue(s)
      case _         => StringValue(obj.toString)
    }
  }

  def fromEnum(obj: Any, enumType: CwlEnum): StringValue = {
    obj match {
      case symbol: String if enumType.symbols.contains(symbol) => StringValue(symbol)
      case _ =>
        throw new Exception(s"symbol ${obj} is not one of ${enumType.symbols.mkString(",")}")
    }
  }

  def deserialize(jsValue: JsValue): StringValue = {
    jsValue match {
      case JsString(s) => StringValue(s)
      case _           => StringValue(jsValue.prettyPrint)
    }
  }

  @tailrec
  def deserialize(jsValue: JsValue, enumType: CwlEnum): StringValue = {
    jsValue match {
      case JsString(symbol) if enumType.symbols.contains(symbol) =>
        StringValue(symbol)
      case JsObject(fields) if fields.contains("symbol") =>
        deserialize(fields("symbol"), enumType)
      case _ =>
        throw new Exception(s"invalid ${enumType} value ${jsValue}")
    }
  }
}

case class BooleanValue(value: Boolean) extends PrimitiveValue {
  override val cwlType: CwlType = CwlBoolean

  override lazy val toJson: JsValue = JsBoolean(value)

  override def toString: String = value.toString

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case CwlString => StringValue(value.toString)
      case _         => throw new RuntimeException
    }
  }
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

  def deserialize(jsValue: JsValue): BooleanValue = {
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

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case CwlInt    => IntValue(decimalValue.toIntExact)
      case CwlLong   => LongValue(decimalValue.toLongExact)
      case CwlFloat  => FloatValue(decimalValue.floatValue)
      case CwlDouble => DoubleValue(decimalValue.doubleValue)
      case CwlString => StringValue(toString)
      case _         => throw new RuntimeException
    }
  }
}

case class IntValue(value: Int) extends NumericValue {
  override val cwlType: CwlType = CwlInt

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)

  override def toString: String = value.toString
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
        throw new Exception(s"cannot convert ${obj} to int", ex)
    }
  }

  def deserialize(jsValue: JsValue): IntValue = {
    try {
      jsValue match {
        case JsNumber(n) => IntValue(n.intValue)
        case JsString(s) => IntValue(s.toInt)
        case _ =>
          throw new Exception(s"invalid int value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new Exception(s"cannot convert ${jsValue} to int", ex)
    }
  }
}

case class LongValue(value: Long) extends NumericValue {
  override val cwlType: CwlType = CwlLong

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)

  override def toString: String = value.toString
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
        throw new Exception(s"cannot convert ${obj} to long", ex)
    }
  }

  def deserialize(jsValue: JsValue): LongValue = {
    try {
      jsValue match {
        case JsNumber(n) => LongValue(n.longValue)
        case JsString(s) => LongValue(s.toLong)
        case _ =>
          throw new Exception(s"invalid long value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new Exception(s"cannot convert ${jsValue} to long", ex)
    }
  }
}

case class FloatValue(value: Float) extends NumericValue {
  override val cwlType: CwlType = CwlFloat

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)

  override def toString: String = value.toString
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
        throw new Exception(s"cannot convert ${obj} to float", ex)
    }
  }

  def deserialize(jsValue: JsValue): FloatValue = {
    try {
      jsValue match {
        case JsNumber(n) => FloatValue(n.floatValue)
        case JsString(s) => FloatValue(s.toFloat)
        case _ =>
          throw new Exception(s"invalid float value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new Exception(s"cannot convert ${jsValue} to float", ex)
    }
  }
}

case class DoubleValue(value: Double) extends NumericValue {
  override val cwlType: CwlType = CwlDouble

  override lazy val toJson: JsValue = JsNumber(value)

  override lazy val decimalValue: BigDecimal = BigDecimal.apply(value)

  override def toString: String = value.toString
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
        throw new Exception(s"cannot convert ${obj} to double", ex)
    }
  }

  def deserialize(jsValue: JsValue): DoubleValue = {
    try {
      jsValue match {
        case JsNumber(n) => DoubleValue(n.doubleValue)
        case JsString(s) => DoubleValue(s.toDouble)
        case _ =>
          throw new Exception(s"invalid double value ${jsValue}")
      }
    } catch {
      case ex: ArithmeticException =>
        throw new Exception(s"cannot convert ${jsValue} to double", ex)
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

  override val cwlType: CwlPath

  def productElementNames: Iterator[String]

  override lazy val toJson: JsValue = {
    val fields = productElementNames.flatMap { key =>
      get(key) match {
        case None        => None
        case Some(value) => Some(key -> value.toJson)
      }
    }.toMap
    JsObject(fields ++ Map("class" -> JsString(cwlType.className)))
  }

  override lazy val toString: String = location.orElse(path).getOrElse(s"<${this.cwlType} literal>")

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case CwlString => StringValue(toString)
      case _         => throw new RuntimeException
    }
  }
}

object PathValue {
  def apply(expr: java.lang.Object): PathValue = {
    expr match {
      case file: FileImpl           => apply(file)
      case directory: DirectoryImpl => apply(directory)
      case _ =>
        throw new Exception(s"unexpected file/directory value ${expr}")
    }
  }

  def apply(map: java.util.Map[_, _]): PathValue = {
    map.get("class") match {
      case "File"      => FileValue(map)
      case "Directory" => DirectoryValue(map)
      case null =>
        throw new Exception(s"missing required key 'class' in expression ${map}")
      case _ => throw new Exception(s"invalid path map ${map}")
    }
  }

  def deserialize(jsValue: JsValue): PathValue = {
    jsValue match {
      case obj: JsObject if obj.fields.contains("class") =>
        obj.fields("class") match {
          case JsString("File")      => FileValue.deserialize(obj)
          case JsString("Directory") => DirectoryValue.deserialize(obj)
          case _ =>
            throw new Exception(s"invalid path object ${jsValue}")
        }
      case JsString(path) => FileValue(location = Some(path))
      case _ =>
        throw new Exception(s"invalid path value ${jsValue}")
    }
  }

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

  def isDirectory(pathValue: PathValue): Boolean = {
    pathValue match {
      case _: DirectoryValue => true
      case _                 => false
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
  override val cwlType: CwlPath = CwlFile
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
        translateOptionalObject(file.getSize).map {
          case n: java.lang.Number => n.longValue()
          case other =>
            throw new Exception(s"invalid size value ${other}")
        },
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
                throw new Exception(s"unexpected path value ${other}")
            }.toVector
          case other =>
            throw new Exception(s"unexpected secondaryFiles value ${other}")
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
        throw new Exception(s"invalid value ${obj}")
    }
  }

  def deserialize(jsValue: JsValue): FileValue = {
    jsValue match {
      case JsString(uri) => FileValue(location = Some(uri))
      case JsObject(fields) =>
        FileValue(
            fields.get("location").map(PathValue.unwrapString),
            fields.get("path").map(PathValue.unwrapString),
            fields.get("basename").map(PathValue.unwrapString),
            fields.get("dirname").map(PathValue.unwrapString),
            fields.get("nameroot").map(PathValue.unwrapString),
            fields.get("nameext").map(PathValue.unwrapString),
            fields.get("checksum").map(PathValue.unwrapString),
            fields.get("size").map(PathValue.unwrapLong),
            fields.get("secondaryFiles") match {
              case Some(JsArray(array)) => array.map(PathValue.deserialize)
              case None                 => Vector.empty
              case other =>
                throw new Exception(s"invalid secondaryFiles value ${other}")
            },
            fields.get("format").map(PathValue.unwrapString),
            fields.get("contents").map(PathValue.unwrapString)
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
  override val cwlType: CwlPath = CwlDirectory
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
                throw new Exception(s"unexpected path value ${other}")
            }.toVector
          case other =>
            throw new Exception(s"unexpected secondaryFiles value ${other}")
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

  def deserialize(jsValue: JsValue): DirectoryValue = {
    jsValue match {
      case JsString(uri) => DirectoryValue(location = Some(uri))
      case JsObject(fields) =>
        DirectoryValue(
            fields.get("location").map(PathValue.unwrapString),
            fields.get("path").map(PathValue.unwrapString),
            fields.get("basename").map(PathValue.unwrapString),
            fields.get("listing") match {
              case Some(JsArray(entries)) => entries.map(PathValue.deserialize)
              case None                   => Vector.empty
              case other =>
                throw new Exception(s"invalid listing ${other}")
            }
        )
      case _ =>
        throw new Exception(s"invalid file value ${jsValue}")
    }
  }
}

/**
  * Indicates that a random filename should be generated to store the
  * contents of stdout/stderr.
  */
case class RandomFile(stdfile: StdFile.StdFile) extends PrimitiveValue {
  override val cwlType: CwlType = CwlFile

  override def toString: String = stdfile.toString

  override def toJson: JsValue = {
    JsObject("location" -> JsString(toString))
  }

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case CwlString => StringValue(toString)
      case _         => throw new RuntimeException
    }
  }
}

sealed trait ContainerValue extends CwlValue {
  protected def coerceToOther(targetType: CwlType): CwlValue

  override def coerceTo(targetType: CwlType): CwlValue = {
    targetType match {
      case CwlAny         => this
      case CwlOptional(t) => coerceTo(t)
      case CwlMulti(types) =>
        types.iterator
          .map { t =>
            try {
              Some(coerceTo(t))
            } catch {
              case _: Throwable => None
            }
          }
          .collectFirst {
            case Some(result) => result
          }
          .getOrElse(
              throw new Exception(
                  s"cannot coerce ${this} to any of types ${types.mkString(",")}"
              )
          )
      case _ if coercibleTo(targetType) => coerceToOther(targetType)
      case _ =>
        throw new Exception(s"${this} is not coercible to ${targetType}")
    }
  }
}

case class ArrayValue(items: Vector[CwlValue]) extends ContainerValue with IntIndexable {
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
      case schema: CwlArray => items.forall(_.coercibleTo(schema.itemType))
      case _                => false
    }
  }

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case arrayType: CwlArray if coercibleTo(arrayType) =>
        ArrayValue(items.map(_.coerceTo(arrayType.itemType)))
      case _ =>
        throw new Exception(s"${this} is not coercible to ${targetType}")
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
          CwlValue(obj.asInstanceOf[java.lang.Object], cwlType.itemType, schemaDefs)
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

  @tailrec
  def deserialize(jsValue: JsValue,
                  schema: CwlArray,
                  schemaDefs: Map[String, CwlSchema]): ArrayValue = {
    jsValue match {
      case JsArray(array) =>
        val (_, values) = array.map(CwlValue.deserialize(_, schema.itemType, schemaDefs)).unzip
        ArrayValue(values)
      case JsObject(fields) if fields.contains("values") =>
        fields("values") match {
          case array: JsArray => deserialize(array, schema, schemaDefs)
          case _ =>
            throw new Exception(s"invalid array value ${jsValue}")
        }
      case _ =>
        throw new Exception(s"invalid array value ${jsValue}")
    }
  }
}

trait ObjectLike extends CwlValue with StringIndexable {
  def fields: SeqMap[String, CwlValue]
}

case class ObjectValue(fields: SeqMap[String, CwlValue]) extends ContainerValue with ObjectLike {

  override def contains(key: String): Boolean = {
    fields.contains(key)
  }

  override def get(key: String): Option[CwlValue] = {
    fields.get(key)
  }

  override def toJson: JsValue = {
    CwlValue.serializeMap(fields)
  }

  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
      case CwlAny => true
      case schema: CwlRecord =>
        schema.fields.values.forall { field =>
          if (fields.contains(field.name)) {
            val value = fields(field.name)
            value.coercibleTo(field.cwlType)
          } else if (field.optional) {
            true
          } else {
            false
          }
        }
      case _ => false
    }
  }

  override def coerceToOther(targetType: CwlType): CwlValue = {
    targetType match {
      case schema: CwlRecord if coercibleTo(schema) =>
        ObjectValue(
            schema.fields.values
              .map { field =>
                val value = if (fields.contains(field.name)) {
                  fields(field.name).coerceTo(field.cwlType)
                } else if (field.optional) {
                  NullValue
                } else {
                  throw new RuntimeException
                }
                field.name -> value
              }
              .to(TreeSeqMap)
        )
      case _ =>
        throw new Exception(s"${this} is not coercible to ${targetType}")
    }
  }
}

object ObjectValue {
  lazy val empty: ObjectValue = ObjectValue(SeqMap.empty)

  def apply(map: java.util.Map[_, _], schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    ObjectValue(
        map.asScala
          .map {
            case (key: String, value) =>
              key -> CwlValue(value.asInstanceOf[java.lang.Object], schemaDefs)
            case other =>
              throw new Exception(s"invalid object member ${other}")
          }
          .to(TreeSeqMap)
    )
  }

  def apply(map: java.util.Map[_, _],
            schema: CwlRecord,
            schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    ObjectValue(
        schema.fields.view
          .mapValues { field =>
            map.get(field.name) match {
              case null if field.optional => NullValue
              case null =>
                throw new Exception(s"missing required field ${field.name} in ${map}")
              case obj: java.lang.Object =>
                CwlValue(obj, field.cwlType, schemaDefs)
            }
          }
          .to(TreeSeqMap)
    )
  }

  def apply(obj: Any, cwlType: CwlRecord, schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    obj match {
      case map: java.util.Map[_, _] =>
        apply(map, cwlType, schemaDefs)
      case _ =>
        throw new Exception(s"invalid record value ${obj}")
    }
  }

  def deserialize(jsValue: JsValue,
                  schema: CwlRecord,
                  schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    val fields = jsValue.asJsObject.fields
    ObjectValue(
        schema.fields.view
          .mapValues { field =>
            if (fields.contains(field.name)) {
              val (_, value) = CwlValue.deserialize(fields(field.name), field.cwlType, schemaDefs)
              value
            } else if (field.optional) {
              NullValue
            } else {
              throw new Exception(s"missing required field ${field.name}")
            }
          }
          .to(TreeSeqMap)
    )
  }
}
