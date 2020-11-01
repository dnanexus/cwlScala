import dx.cwl._

object EvalutatorExample {
  val evaluator: Evaluator = Evaluator(jsEnabled = true)
  val ctx: EvaluatorContext = EvaluatorContext(inputs = ObjectValue(
      Map(
          "name" -> StringValue("Ned")
      )
  )
  )
  val msg: String = evaluator.applyString("Hello $(inputs.name)!")
  println(msg)
}
