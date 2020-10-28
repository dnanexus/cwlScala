package dx.js

import org.mozilla.javascript._
import spray.json._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

object NativeJavaObj {
  def unapply(x: Any): Option[Any] = x match {
    case w: Wrapper => Some(w.unwrap())
    case _          => None
  }
}

trait JsonSupport {
  def toScala[T: JsonReader](input: Any)(implicit tag: ClassTag[T]): Option[T] = {
    input match {
      case NativeJavaObj(x: T) => Some(x)
      case o =>
        toJsValueOption(o).flatMap { jsValue =>
          try {
            val converted = jsValue.convertTo[T]
            Some(converted)
          } catch {
            case _: DeserializationException => None
            case e: Throwable                => throw e
          }
        }
    }
  }

  def toJsValueOption(input: Any): Option[JsValue] = {
    toJsValue(input) match {
      case value if value == JsNull => None
      case jsValue @ _              => Some(jsValue)
    }
  }

  @tailrec
  private def toJsValue(input: Any): JsValue = input match {
    case b: Boolean => JsBoolean(b)
    case i: Int     => JsNumber(i)
    case l: Long    => JsNumber(l)
    case f: Float   => JsNumber(f)
    case d: Double  => JsNumber(d)
    case s: String  => JsString(s)

    case o: NativeObject => toJsObject(o)
    case a: NativeArray  => toJsArray(a)
    case w: Wrapper      => toJsValue(w.unwrap())

    case _: Undefined => JsNull
    case null         => JsNull
    case _ => {
      JsNull
    }
  }

  private def toJsObject(nativeObject: NativeObject): JsObject = {
    new JsObject(
        nativeObject.entrySet.asScala
          .map(entry => (entry.getKey.toString, toJsValue(entry.getValue)))
          .toMap
    )
  }

  private def toJsArray(nativeArray: NativeArray): JsArray = {
    new JsArray(nativeArray.asScala.map(item => toJsValue(item)).toVector)
  }
}

object JsonSupport {
  implicit object JsObjectReader extends JsonReader[JsObject] {
    def read(value: JsValue): JsObject = value match {
      case o: JsObject => o
      case x           => deserializationError("Expected JsObject, but got " + x)
    }
  }

  implicit object JsArrayReader extends JsonReader[JsArray] {
    def read(value: JsValue): JsArray = value match {
      case o: JsArray => o
      case x          => deserializationError("Expected JsArray, but got " + x)
    }
  }
}
