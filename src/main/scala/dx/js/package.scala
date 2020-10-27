package dx

import org.mozilla.javascript.{Context, EvaluatorException}

package object js {
  def withContext[T](block: Context => T): Option[T] = {
    val context = Context.enter()
    try {
      Option(block(context))
    } catch {
      case _: EvaluatorException => None
      case _: Exception          => None
    } finally {
      Context.exit()
    }
  }
}
