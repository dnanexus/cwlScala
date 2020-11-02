package dx.js

import org.mozilla.javascript.{Context, Scriptable, WrapFactory}
import spray.json.JsonReader

import scala.reflect.ClassTag

/**
  * Javascript evaluation engine, based on Rhino.
  * @param scope the evaluation scope - defaults to the scope with only standard objects defined
  */
case class Engine(scope: Scope = Scope.standard) extends EvaluationSupport with JsonSupport {

  /**
    * Makes an object available to the javascript engine so that it can be referenced
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
      Context.javaToJS(toScala[T](obj), scope)
    }
  }
}
