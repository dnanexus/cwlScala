package dx.cwl

import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{DirectoryImpl, FileImpl}
import spray.json._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

sealed trait CwlValue {
  def toJson: JsValue

  def coercibleTo(targetType: CwlType): Boolean

  def coercibleTo(targetTypes: Vector[CwlType]): Boolean = {
    targetTypes.exists(coercibleTo)
  }
}

sealed trait PrimitiveValue extends CwlValue {
  val cwlType: CwlType

  def coercibleTo(targetType: CwlType): Boolean = {
    cwlType.coercibleTo(targetType)
  }
}

object CwlValue {
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
        throw new RuntimeException(s"cannot translate value without a type specification ${value}")
    }
  }

  def apply(value: java.lang.Object,
            cwlType: CwlType,
            schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlType match {
      case CwlNull if value == null => NullValue
      case CwlNull =>
        throw new Exception("no value allowed for type null")
      case CwlOptional(_) if value == null => NullValue
      case CwlOptional(t)                  => apply(value, t, schemaDefs)
      case CwlString                       => StringValue(value)
      case CwlBoolean                      => BooleanValue(value)
      case CwlInt                          => IntValue(value)
      case CwlLong                         => LongValue(value)
      case CwlFloat                        => FloatValue(value)
      case CwlDouble                       => DoubleValue(value)
      case CwlFile                         => FileValue(value)
      case CwlDirectory                    => DirectoryValue(value)
      case arraySchema: CwlArray           => ArrayValue(value, arraySchema, schemaDefs)
      case recordSchema: CwlRecord         => ObjectValue(value, recordSchema, schemaDefs)
      case enumSchema: CwlEnum             => StringValue.fromEnum(value, enumSchema)
      case _ =>
        throw new RuntimeException(s"cannot translate ${value} to value of type ${cwlType}")
    }
  }

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

  def serializeMap(map: Map[String, CwlValue]): JsObject = {
    JsObject(map.view.mapValues(_.toJson).toMap)
  }

  def deserialize(jsValue: JsValue, schemaDefs: Map[String, CwlSchema]): CwlValue = {
    jsValue match {
      case JsNull       => NullValue
      case JsString(s)  => StringValue(s)
      case JsBoolean(b) => BooleanValue(b)
      case JsNumber(n) =>
        try {
          LongValue(n.toLongExact)
        } catch {
          case _: ArithmeticException => DoubleValue(n.toDouble)
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
        throw new Exception(s"cannot deserialize value ${jsValue}")
    }
  }

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
      case CwlNull if jsValue == JsNull =>
        NullValue
      case CwlOptional(_) if jsValue == JsNull =>
        NullValue
      case CwlOptional(t) =>
        deserialize(jsValue, t, schemaDefs)
      case _ =>
        throw new Exception(s"cannot deserialize ${jsValue} as type ${cwlType}")
    }
  }

  def deserialize(jsValue: JsValue,
                  cwlTypes: Vector[CwlType],
                  schemaDefs: Map[String, CwlSchema]): CwlValue = {
    cwlTypes.iterator
      .map { t =>
        try {
          Some(deserialize(jsValue, t, schemaDefs))
        } catch {
          case _: Throwable => None
        }
      }
      .collectFirst {
        case Some(value) => value
      }
      .getOrElse(
          throw new Exception(
              s"cannot deserialize ${jsValue} to any of types ${cwlTypes.mkString(",")}"
          )
      )
  }
}

sealed trait StringIndexable {
  def get(key: String): Option[CwlValue]
}

sealed trait IntIndexable {
  def get(index: Int): Option[CwlValue]
}

case object NullValue extends PrimitiveValue {
  val cwlType: CwlType = CwlNull

  override val toJson: JsValue = JsNull
}

case class StringValue(value: String) extends PrimitiveValue with IntIndexable {
  val cwlType: CwlType = CwlString

  def get(index: Int): Option[CwlValue] = {
    Some(StringValue(value.charAt(index).toString))
  }

  override lazy val toJson: JsValue = JsString(value)
}

object StringValue {
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
  val cwlType: CwlType = CwlBoolean

  override lazy val toJson: JsValue = JsBoolean(value)
}

object BooleanValue {
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

case class IntValue(value: Int) extends PrimitiveValue {
  val cwlType: CwlType = CwlInt

  override lazy val toJson: JsValue = JsNumber(value)
}

object IntValue {
  def apply(obj: Any): IntValue = {
    try {
      obj match {
        case i: java.lang.Integer =>
          IntValue(i.intValue())
        case l: java.lang.Long =>
          IntValue(new java.math.BigInteger(l.toString).intValueExact())
        case f: java.lang.Float =>
          IntValue(new java.math.BigDecimal(f.toString).intValueExact())
        case d: java.lang.Double =>
          IntValue(new java.math.BigDecimal(d.toString).intValueExact())
        case bi: java.math.BigInteger =>
          IntValue(bi.intValueExact())
        case bd: java.math.BigDecimal =>
          IntValue(bd.intValueExact())
        case s: String =>
          IntValue(s.toInt)
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

case class LongValue(value: Long) extends PrimitiveValue {
  val cwlType: CwlType = CwlLong

  override lazy val toJson: JsValue = JsNumber(value)
}

object LongValue {
  def apply(obj: Any): LongValue = {
    try {
      obj match {
        case i: java.lang.Integer =>
          LongValue(i.intValue())
        case l: java.lang.Long =>
          LongValue(l.longValue())
        case f: java.lang.Float =>
          LongValue(new java.math.BigDecimal(f.toString).longValueExact())
        case d: java.lang.Double =>
          LongValue(new java.math.BigDecimal(d.toString).longValueExact())
        case bi: java.math.BigInteger =>
          LongValue(bi.longValueExact())
        case bd: java.math.BigDecimal =>
          LongValue(bd.longValueExact())
        case s: String =>
          LongValue(s.toLong)
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

case class FloatValue(value: Float) extends PrimitiveValue {
  val cwlType: CwlType = CwlFloat

  override lazy val toJson: JsValue = JsNumber(value)
}

object FloatValue {
  def apply(obj: Any): LongValue = {
    try {
      obj match {
        case i: java.lang.Integer =>
          FloatValue(i.floatValue())
        case l: java.lang.Long =>
          FloatValue(l.floatValue())
        case f: java.lang.Float =>
          FloatValue(f.floatValue())
        case d: java.lang.Double =>
          FloatValue(new java.math.BigDecimal(d.toString).floatValue())
        case bi: java.math.BigInteger =>
          FloatValue(bi.floatValue())
        case bd: java.math.BigDecimal =>
          FloatValue(bd.floatValue())
        case s: String =>
          FloatValue(s.toFloat)
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

case class DoubleValue(value: Double) extends PrimitiveValue {
  val cwlType: CwlType = CwlDouble

  override lazy val toJson: JsValue = JsNumber(value)
}

object DoubleValue {
  def apply(obj: Any): DoubleValue = {
    try {
      obj match {
        case (CwlDouble, i: java.lang.Integer)     => DoubleValue(i.doubleValue())
        case (CwlDouble, l: java.lang.Long)        => DoubleValue(l.doubleValue())
        case (CwlDouble, f: java.lang.Float)       => DoubleValue(f.doubleValue())
        case (CwlDouble, d: java.lang.Double)      => DoubleValue(d.doubleValue())
        case (CwlDouble, bi: java.math.BigInteger) => DoubleValue(bi.doubleValue())
        case (CwlDouble, bd: java.math.BigDecimal) => DoubleValue(bd.doubleValue())
        case s: String                             => DoubleValue(s.toDouble)
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
  val cwlType: CwlType = CwlFile

  def get(key: String): Option[CwlValue] = {
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
    }
  }
}

case class DirectoryValue(location: Option[String] = None,
                          path: Option[String] = None,
                          basename: Option[String] = None,
                          listing: Vector[PathValue] = Vector.empty)
    extends PathValue {
  val cwlType: CwlType = CwlDirectory

  def get(key: String): Option[CwlValue] = {
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
  val cwlType: CwlType = CwlFile

  override def toJson: JsValue = {
    JsObject("location" -> JsString(stdfile.toString))
  }
}

case class ArrayValue(items: Vector[CwlValue]) extends CwlValue with IntIndexable {
  def get(index: Int): Option[CwlValue] = {
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
  def apply(array: java.util.Collection[_], schemaDefs: Map[String, CwlSchema]): ArrayValue = {
    ArrayValue(
        array.asScala.toVector.map(obj => CwlValue(obj.asInstanceOf[java.lang.Object], schemaDefs))
    )
  }

  def apply(array: java.util.Collection[_],
            cwlType: CwlArray,
            schemaDefs: Map[String, CwlSchema] = Map.empty): ArrayValue = {
    ArrayValue(
        array.asScala.toVector.map(obj =>
          CwlValue(obj.asInstanceOf[java.lang.Object], cwlType.itemTypes, schemaDefs)
        )
    )
  }

  def apply(obj: Any,
            cwlType: CwlArray,
            schemaDefs: Map[String, CwlSchema] = Map.empty): ArrayValue = {
    obj match {
      case array: java.util.Collection[_] =>
        apply(array, cwlType, schemaDefs)
      case _ =>
        throw new Exception(s"invalid array value ${obj}")
    }
  }

  def apply(jsValue: JsValue,
            schema: CwlArray,
            schemaDefs: Map[String, CwlSchema] = Map.empty): ArrayValue = {
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

case class ObjectValue(members: Map[String, CwlValue]) extends CwlValue with StringIndexable {
  def get(key: String): Option[CwlValue] = {
    members.get(key)
  }

  override def toJson: JsValue = {
    JsObject(members.view.mapValues(_.toJson).toMap)
  }

  override def coercibleTo(targetType: CwlType): Boolean = {
    targetType match {
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
    }
  }
}

object ObjectValue {
  def apply(map: java.util.Map[_, _], schemaDefs: Map[String, CwlSchema]): ObjectValue = {
    ObjectValue(map.asScala.map {
      case (key: String, value) =>
        key -> CwlValue(value.asInstanceOf[java.lang.Object], schemaDefs)
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
      }value
    }.toMap)
  }

  def apply(obj: Any,
            cwlType: CwlRecord,
            schemaDefs: Map[String, CwlSchema] = Map.empty): ObjectValue = {
    obj match {
      case map: java.util.Map[_, _] =>
        apply(map, cwlType, schemaDefs)
      case _ =>
        throw new Exception(s"invalid record value ${obj}")
    }
  }

  def apply(jsValue: JsValue,
            schema: CwlRecord,
            schemaDefs: Map[String, CwlSchema] = Map.empty): ObjectValue = {
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
