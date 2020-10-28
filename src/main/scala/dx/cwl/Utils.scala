package dx.cwl

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

object Utils {
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
}
