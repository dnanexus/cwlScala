# Scala CWL library

cwlScala is a Scala wrapper around [cwljava v1.2](https://github.com/common-workflow-lab/cwljava/issues), the Java parser for the [Common Workflow Langauge](https://www.commonwl.org/). Also included is an evaluation engine for CWL expressions (which are ECMAscript 5.1), which is based on the [rhinos](https://github.com/agemooij/rhinos) wrapper of the [Mozilla Rhino](https://github.com/mozilla/rhino) JavaScript engine.

## Examples

### Parsing a CWL CommandLineTool

```scala
// parsing a CommandLineTool
import java.nio.file.Paths
import dx.cwl.{CommandLineTool, Parser}
object ParserExample {
  val tool: CommandLineTool = Parser.parse(Paths.get("../../test/resources/tools/pass/action.cwl")) match {
    case tool: CommandLineTool => tool
    case _                     => throw new Exception("not a CommandLineTool")
  }
  tool.inputs.map { param =>
    println(s"${param.id.get}: ${param.types}")
  }
}
```

### Evaluating an ECMAscript expression

```scala
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
```

## Requirements

* JDK 8
* Scala 2.13
* sbt

## Using

cwlScala is published in the Maven repository. You can include it in your build.sbt file like so:

```
val cwlScalaVersion = "0.1.0"
libraryDependencies ++= Seq(
    "com.dnanexus" % "cwlScala" % cwlScalaVersion
)
```

## Building

```
$ git clone --recurse-submodules https://github.com/dnanexus/cwlScala.git
$ sbt publishLocal
```
