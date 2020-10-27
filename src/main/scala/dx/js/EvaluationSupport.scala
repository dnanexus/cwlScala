package dx.js

import java.io._
import java.nio.file.Path

import org.mozilla.javascript.ScriptableObject
import spray.json._

import scala.language.{implicitConversions, reflectiveCalls}
import scala.reflect.ClassTag
import scala.util.control.Exception._

case class Scope(wrapped: ScriptableObject) {}

object Scope {
  implicit def objToScope(scope: ScriptableObject): Scope = Scope(scope)
  implicit def scopeToObj(rhinosScope: Scope): ScriptableObject = rhinosScope.wrapped
}

trait EvaluationSupport { self: JsonSupport =>
  protected val scope: Scope

  def eval[T: JsonReader](javascriptCode: String)(implicit tag: ClassTag[T]): Option[T] = {
    withContext[Any] { context =>
      context.evaluateString(scope, javascriptCode, "RhinoContext.eval(String)", 1, null)
    }.flatMap(value => toScala[T](value))
  }

  def evalReader[T: JsonReader](reader: Reader)(implicit tag: ClassTag[T]): Option[T] = {
    using(reader) { r =>
      withContext[Any] { context =>
        context.evaluateReader(scope, r, "RhinoContext.eval(Reader)", 1, null)
      }
    }.flatMap(value => toScala[T](value))
  }

  def evalFile[T: JsonReader](path: String)(implicit tag: ClassTag[T]): Option[T] = {
    evalFile(new File(path))
  }

  def evalFile[T: JsonReader](path: Path)(implicit tag: ClassTag[T]): Option[T] = {
    evalFile(path.toFile)
  }

  def evalFile[T: JsonReader](file: File)(implicit tag: ClassTag[T]): Option[T] = {
    if (file != null && file.exists) {
      evalReader(new FileReader(file))
    } else {
      None
    }
  }

  def evalFileOnClasspath[T: JsonReader](path: String)(implicit tag: ClassTag[T]): Option[T] = {
    val in = this.getClass.getClassLoader.getResourceAsStream(path)
    if (in != null) {
      evalReader(new BufferedReader(new InputStreamReader(in)))
    } else {
      None
    }
  }

  private def using[X <: { def close(): Unit }, A](resource: X)(f: X => A): A = {
    try {
      f(resource)
    } finally {
      ignoring(classOf[Exception]) {
        resource.close()
      }
    }
  }
}
