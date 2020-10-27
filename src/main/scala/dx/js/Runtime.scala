package dx.js

import org.mozilla.javascript.{Context, Scriptable, WrapFactory}
import spray.json.JsonReader

import scala.reflect.ClassTag

case class Runtime(
    scope: Scope = withContext[Scope](_.initStandardObjects()).get
) extends EvaluationSupport
    with JsonSupport {

  /**
    * Makes an object available to javascript so that it can be called off to
    * @param name object name
    * @param callbackObj callback
    */
  def addObject(name: String, callbackObj: Any): Unit = {
    withContext { _ =>
      val jsobj = Context.javaToJS(callbackObj, scope)
      scope.put(name, scope.wrapped, jsobj)
    }
  }

  class JsWrapFactory[T: JsonReader](implicit tag: ClassTag[T])
      extends WrapFactory
      with JsonSupport {
    override def wrap(cx: Context, scope: Scriptable, obj: Any, staticType: Class[_]): Any = {
      println("trying to wrap up:" + obj)
      Context.javaToJS(toScala[T](obj), scope)
    }
  }
}
