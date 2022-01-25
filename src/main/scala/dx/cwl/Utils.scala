package dx.cwl

import dx.yaml.{
  YamlArray,
  YamlBoolean,
  YamlNaN,
  YamlNegativeInf,
  YamlNull,
  YamlNumber,
  YamlObject,
  YamlPositiveInf,
  YamlSet,
  YamlString,
  YamlValue
}
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}

import java.net.URI
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

object Utils {
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

  def translateString(obj: Any): String = {
    obj match {
      case s: String => s
      case _         => throw new RuntimeException(s"unexpected string value ${obj}")
    }
  }

  def translateOptionalString(obj: Any): Option[String] = {
    obj match {
      case null      => None
      case s: String => Some(s)
      case _         => throw new RuntimeException(s"unexpected string value ${obj}")
    }
  }

  def translateInt(obj: Any): Int = {
    obj match {
      case i: java.lang.Integer => i.toInt
      case _ =>
        throw new RuntimeException(s"unexpected int value ${obj}")
    }
  }

  def translateDoc(obj: Any): Option[String] = {
    obj match {
      case null                 => None
      case s: String            => Some(s)
      case a: java.util.List[_] => Some(a.asScala.mkString("\n"))
      case _ =>
        throw new RuntimeException(s"unexpected doc value ${obj}")
    }
  }

  def toStringAnyMap(m: Map[_, _]): Map[String, Any] = {
    m.map {
      case (k: String, v) => k -> v
      case (other, _)     => throw new Exception(s"expected string key, not ${other}")
    }
  }

  private val identifierRegexp = "(.*?)(?:#(.*))?".r

  def splitUri(uri: String): (Option[String], Option[String]) = {
    uri match {
      case identifierRegexp(frag, null)                           => (None, Some(frag))
      case identifierRegexp(namespace, frag) if frag.isEmpty      => (Some(namespace), None)
      case identifierRegexp(namespace, frag) if namespace.isEmpty => (None, Some(frag))
      case identifierRegexp(namespace, id)                        => (Some(namespace), Some(id))
      case _                                                      => throw new Exception(s"invalid URI ${uri}")
    }
  }

  def normalizeAndSplitUri(uri: URI): (Option[String], Option[String]) = {
    try {
      (Option(uri.getScheme), Option(uri.getFragment)) match {
        case (Some("file"), None)       => (Some(s"file:${uri.getPath}"), None)
        case (Some("file"), Some(frag)) => (Some(s"file:${uri.getPath}"), Some(frag))
        case (Some(_), None)            => (Some(uri.toString), None)
        case _                          => splitUri(uri.toString)
      }
    } catch {
      case _: IllegalArgumentException => (None, Some(uri.toString))
    }
  }

  def normalizeUri(uri: URI): String = {
    normalizeAndSplitUri(uri) match {
      case (Some(base), Some(frag)) => s"${base}#${frag}"
      case (Some(base), None)       => base
      case (None, _)                => throw new Exception(s"invalid uri ${uri}")
    }
  }

  def yamlToJson(yamlValue: YamlValue): JsValue = {
    yamlValue match {
      case YamlNull        => JsNull
      case YamlString(s)   => JsString(s)
      case YamlBoolean(b)  => JsBoolean(b)
      case YamlNumber(n)   => JsNumber(n)
      case YamlNaN         => JsNumber(Double.NaN)
      case YamlPositiveInf => JsNumber(Double.PositiveInfinity)
      case YamlNegativeInf => JsNumber(Double.NegativeInfinity)
      case YamlArray(a)    => JsArray(a.map(yamlToJson))
      case YamlSet(s)      => JsArray(s.map(yamlToJson).toVector)
      case YamlObject(obj) =>
        JsObject(obj.map {
          case (YamlString(key), value) => key -> yamlToJson(value)
          case (key, value)             => key.toString -> yamlToJson(value)
        })
      case other =>
        throw new Exception(s"unexpected value ${other}")
    }
  }
}
