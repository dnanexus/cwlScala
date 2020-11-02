package dx.js

import java.io._
import java.nio.file.Path

import org.mozilla.javascript.ScriptableObject
import org.mozilla.javascript.json.{JsonParser => RhinoJsonParser}
import spray.json._

import scala.language.{implicitConversions, reflectiveCalls}
import scala.reflect.ClassTag
import scala.util.control.Exception._

case class Scope(wrapped: ScriptableObject) {}

object Scope {
  implicit def objToScope(scope: ScriptableObject): Scope = Scope(scope)
  implicit def scopeToObj(scope: Scope): ScriptableObject = scope.wrapped

  lazy val standard: Scope = {
    withContext[Scope](_.initStandardObjects()).get
  }

  /**
    * Creates a scope that contains the standard values as well as any additional
    * values specified.
    * @param jsValues Additional values to add to the scope, as JSON objects.
    * @return
    */
  def create(jsValues: Map[String, JsValue] = Map.empty): Scope = {
    withContext[Scope] { ctx =>
      val newScope = ctx.newObject(standard.wrapped)
      newScope.setPrototype(standard.wrapped)
      newScope.setParentScope(null)
      if (jsValues.nonEmpty) {
        val parser = new RhinoJsonParser(ctx, newScope)
        jsValues.foreach {
          case (name, jsValue) =>
            newScope.put(name, newScope, parser.parseValue(jsValue.prettyPrint))
        }
      }
      Scope(newScope.asInstanceOf[ScriptableObject])
    }.get
  }
}

trait EvaluationSupport { self: JsonSupport =>
  protected val scope: Scope

  def evalToJson(javascriptCode: String): Option[JsValue] = {
    withContext[Any] { context =>
      context.evaluateString(scope, javascriptCode, "RhinoContext.eval(String)", 1, null)
    }.flatMap(value => toJsValueOption(value))
  }

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
