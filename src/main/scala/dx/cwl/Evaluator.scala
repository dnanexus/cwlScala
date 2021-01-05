package dx.cwl

import java.lang.management.ManagementFactory
import java.nio.ByteBuffer
import java.nio.file.{Path, Paths}
import java.lang.{Runtime => JavaRuntime}
import java.security.AccessController.doPrivileged
import dx.js.{Engine, Scope}
import org.antlr.v4.runtime.{CodePointBuffer, CodePointCharStream, CommonTokenStream}
import org.commonwl.cwl.ecma.v1_2.{CwlEcmaStringLexer, CwlEcmaStringParser}
import org.commonwl.cwl.refparser.v1_2.{CwlParameterReferenceLexer, CwlParameterReferenceParser}
import spray.json._
import sun.security.action.GetPropertyAction

import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

/**
  * Marker trait for expressions, which may be literal values,
  * parameter references, or javascript expressions that require
  * evaluation within a runtime context.
  */
sealed trait ParameterValue

/**
  * A literal value
  */
case class LiteralValue(value: CwlValue) extends ParameterValue {
  override def toString: String = {
    value.toString
  }
}

/**
  * A string that includes placeholders.
  * @param parts the parts of the string - alternating `StringValue` and `CwlExpr` objects.
  * @example
  * {{{
  * "my name is \${name}" -> CompoundString(StringValue("my name is "), EcmaExpr("name"))
  * }}}
  */
case class CompoundValue(parts: Vector[ParameterValue]) extends ParameterValue

sealed trait ParameterReferenceSegment
case class Symbol(value: String) extends ParameterReferenceSegment
case class IntIndex(value: Int) extends ParameterReferenceSegment
case class StringIndex(value: String) extends ParameterReferenceSegment

/**
  * A parameter reference.
  * @param rootSymbol the initial symbol of the reference
  * @param segments additional parts of the reference
  */
case class ParameterReference(rootSymbol: Symbol, segments: Vector[ParameterReferenceSegment])
    extends ParameterValue

case class ParameterReferenceParser(trace: Boolean = false) {
  private def visitStringIndexPart(
      ctx: CwlParameterReferenceParser.String_index_partContext
  ): String = {
    if (ctx.StringIndexPart() != null) {
      ctx.StringIndexPart().toString
    } else if (ctx.StringIndexEscPart() != null) {
      val escapedStr = ctx.StringIndexEscPart().toString
      if (!escapedStr.startsWith("\\")) {
        throw new Exception(s"invalid escaped string ${escapedStr}")
      }
      escapedStr.drop(1)
    } else {
      throw new Exception(s"unexpected string index part ${ctx}")
    }
  }

  private def visitSegment(
      ctx: CwlParameterReferenceParser.Expr_segmentContext
  ): ParameterReferenceSegment = {
    if (ctx.expr_dot_symbol() != null) {
      Symbol(ctx.expr_dot_symbol().ExprSymbol().toString)
    } else if (ctx.int_index() != null) {
      IntIndex(ctx.int_index().IntIndexPart().toString.toInt)
    } else if (ctx.string_index() != null) {
      StringIndex(
          ctx.string_index().string_index_part().asScala.map(visitStringIndexPart).mkString("")
      )
    } else {
      throw new Exception("invalid expression segment")
    }
  }

  private def visitInterpolatedString(
      ctx: CwlParameterReferenceParser.Interpolated_stringContext
  ): ParameterValue = {
    if (ctx.interpolated_string_part() == null) {
      throw new Exception("empty value")
    }
    val (parts, buf, hasExpr) = ctx
      .interpolated_string_part()
      .asScala
      .toVector
      .foldLeft((Vector.empty[ParameterValue], new StringBuilder, false)) {
        case ((accu, buf, hasExpr), part) =>
          if (part.paren_expr() != null) {
            if (part.paren_expr().ExprSymbol() == null) {
              throw new Exception("missing root symbol")
            }
            val rootSymbol = Symbol(part.paren_expr().ExprSymbol().toString)
            val segments = if (part.paren_expr().expr_segment() != null) {
              part.paren_expr().expr_segment().asScala.toVector.map(visitSegment)
            } else {
              Vector.empty
            }
            val ref = ParameterReference(rootSymbol, segments)
            if (buf.isEmpty) {
              (accu :+ ref, buf, true)
            } else {
              val stringVal = LiteralValue(StringValue(buf.toString))
              buf.clear()
              (accu ++ Vector(stringVal, ref), buf, true)
            }
          } else {
            if (part.BACKSLASHESC() != null) {
              buf.append("\\")
            } else if (part.DOLLARPARENESC() != null) {
              buf.append("$(")
            } else if (part.ANYCHAR() != null) {
              buf.append(part.ANYCHAR().toString)
            } else {
              throw new Exception("missing token")
            }
            (accu, buf, hasExpr)
          }
      }
    val allParts = if (buf.nonEmpty) {
      parts :+ LiteralValue(StringValue(buf.toString))
    } else {
      parts
    }
    if (parts.size == 1) {
      allParts.head
    } else if (hasExpr) {
      CompoundValue(allParts)
    } else {
      LiteralValue(StringValue(allParts.map(_.toString).mkString("")))
    }
  }

  def apply(s: String): ParameterValue = {
    // the string might contain an expression
    val codePointBuffer: CodePointBuffer =
      CodePointBuffer.withBytes(ByteBuffer.wrap(s.getBytes()))
    val charStream = CodePointCharStream.fromBuffer(codePointBuffer)
    val lexer = new CwlParameterReferenceLexer(charStream)
    val parser = new CwlParameterReferenceParser(new CommonTokenStream(lexer))
    parser.setTrace(trace)
    try {
      visitInterpolatedString(parser.interpolated_string())
    } catch {
      case ex: Throwable =>
        throw new Exception(s"error evaluating string ${s}", ex)
    }
  }
}

object ParameterReferenceParser {
  lazy val default: ParameterReferenceParser = ParameterReferenceParser()
}

trait EcmaString
case class StringLiteral(value: String) extends EcmaString
case class EcmaExpr(value: String) extends EcmaString
case class EcmaFunctionBody(value: String) extends EcmaString
case class CompoundString(parts: Vector[EcmaString]) extends EcmaString

/**
  * Parses a string that may contain ECMA expressions (`\$(...)`) or function bodies
  * (`\${...}`). The expressions themselves are not evaluated (which requires a
  * JavaScript engine).
  */
case class EcmaStringParser(trace: Boolean = false) {
  private def visitSubSubExpr(ctx: CwlEcmaStringParser.Sub_sub_exprContext,
                              prefix: String,
                              suffix: String): String = {
    val subSubExprStr = ctx
      .sub_expr_part()
      .asScala
      .map { subSubPart =>
        if (subSubPart.SubExprPart() != null) {
          subSubPart.SubExprPart().toString
        } else if (subSubPart.sub_sub_expr() != null) {
          visitSubSubExpr(subSubPart.sub_sub_expr(), prefix, suffix)
        } else {
          throw new Exception(s"invalid sub-expression part ${subSubPart}")
        }
      }
      .mkString("")
    s"${prefix}${subSubExprStr}${suffix}"
  }

  private def visitSubExpr(ctx: CwlEcmaStringParser.Sub_exprContext,
                           prefix: String,
                           suffix: String): String = {
    val subExprStr = ctx
      .sub_expr_part()
      .asScala
      .map { subPart =>
        if (subPart.SubExprPart() != null) {
          subPart.SubExprPart().toString
        } else if (subPart.sub_sub_expr() != null) {
          visitSubSubExpr(subPart.sub_sub_expr(), prefix, suffix)
        } else {
          throw new Exception(s"invalid sub-expression part ${subPart}")
        }
      }
      .mkString("")
    s"${prefix}${subExprStr}${suffix}"
  }

  private def visitExprParts(parts: Vector[CwlEcmaStringParser.Expr_partContext],
                             prefix: String,
                             suffix: String): String = {
    parts
      .map { part =>
        if (part.EscPart() != null) {
          val escapedStr = part.EscPart().toString
          if (!escapedStr.startsWith("\\")) {
            throw new Exception(s"invalid escaped string ${escapedStr}")
          }
          escapedStr.drop(1)
        } else if (part.ExprPart() != null) {
          part.ExprPart().toString
        } else if (part.sub_expr() != null) {
          visitSubExpr(part.sub_expr(), prefix, suffix)
        } else {
          throw new Exception(s"invalid expression part ${part}")
        }
      }
      .mkString("")
  }

  private def visitInterpolatedString(
      ctx: CwlEcmaStringParser.Interpolated_stringContext
  ): EcmaString = {
    if (ctx.interpolated_string_part() != null) {
      val (parts, buf, hasExpr) = ctx
        .interpolated_string_part()
        .asScala
        .foldLeft((Vector.empty[EcmaString], new StringBuilder, false)) {
          case ((accu, buf, hasExpr), stringPart) =>
            if (stringPart.paren_expr() != null) {
              val expr = EcmaExpr(
                  visitExprParts(stringPart.paren_expr().expr_part().asScala.toVector, "(", ")")
              )
              if (buf.isEmpty) {
                (accu :+ expr, buf, true)
              } else {
                val stringVal = StringLiteral(buf.toString)
                buf.clear()
                (accu ++ Vector(stringVal, expr), buf, true)
              }
            } else if (stringPart.brace_expr() != null) {
              val expr = EcmaFunctionBody(
                  visitExprParts(stringPart.brace_expr().expr_part().asScala.toVector, "{", "}")
              )
              if (buf.isEmpty) {
                (accu :+ expr, buf, true)
              } else {
                val stringVal = StringLiteral(buf.toString)
                buf.clear()
                (accu ++ Vector(stringVal, expr), buf, true)
              }
            } else {
              if (stringPart.BACKSLASHESC() != null) {
                buf.append("\\")
              } else if (stringPart.DOLLARPARENESC() != null) {
                buf.append("$(")
              } else if (stringPart.DOLLARBRACEESC() != null) {
                buf.append("${")
              } else if (stringPart.ANYCHAR() != null) {
                buf.append(stringPart.ANYCHAR().toString)
              } else {
                throw new Exception("missing token")
              }
              (accu, buf, hasExpr)
            }
        }
      val allParts = if (buf.nonEmpty) {
        parts :+ StringLiteral(buf.toString)
      } else {
        parts
      }
      if (allParts.size == 1) {
        allParts.head
      } else if (hasExpr) {
        CompoundString(allParts)
      } else {
        StringLiteral(allParts.map(_.toString).mkString(""))
      }
    } else {
      throw new Exception("empty string")
    }
  }

  def apply(s: String): EcmaString = {
    // the string might contain an expression
    val codePointBuffer: CodePointBuffer =
      CodePointBuffer.withBytes(ByteBuffer.wrap(s.getBytes()))
    val charStream = CodePointCharStream.fromBuffer(codePointBuffer)
    val lexer = new CwlEcmaStringLexer(charStream)
    val parser = new CwlEcmaStringParser(new CommonTokenStream(lexer))
    parser.setTrace(trace)
    try {
      visitInterpolatedString(parser.interpolated_string())
    } catch {
      case ex: Throwable =>
        throw new Exception(s"error evaluating string ${s}", ex)
    }
  }
}

object EcmaStringParser {
  lazy val default: EcmaStringParser = EcmaStringParser()
}

/**
  * The runtime context to use when evaluating expressions.
  * @param outdir output dir
  * @param tmpdir temp dir
  * @param cores number of available cores
  * @param ram amount of available ram
  * @param outdirSize size of output dir
  * @param tmpdirSize size of temp dir
  */
case class Runtime(outdir: String,
                   tmpdir: String,
                   cores: Int,
                   ram: Long,
                   outdirSize: Long,
                   tmpdirSize: Long)
    extends ObjectLike {
  private val keys: Set[String] = Set(
      "outdir",
      "tmpdir",
      "cores",
      "ram",
      "outdirSize",
      "tmpdirSize"
  )

  override def contains(key: String): Boolean = {
    keys.contains(key)
  }

  override def get(key: String): Option[CwlValue] = {
    key match {
      case "outdir"     => Some(StringValue(outdir))
      case "tmpdir"     => Some(StringValue(tmpdir))
      case "cores"      => Some(IntValue(cores))
      case "ram"        => Some(LongValue(ram))
      case "outdirSize" => Some(LongValue(outdirSize))
      case "tmpdirSize" => Some(LongValue(tmpdirSize))
      case _            => None
    }
  }

  override def members: Map[String, CwlValue] = {
    keys.map(key => key -> apply(key)).toMap
  }

  override def coercibleTo(targetType: CwlType): Boolean = false

  override def toJson: JsValue = {
    JsObject(
        Map(
            "outdir" -> JsString(outdir),
            "tmpdir" -> JsString(tmpdir),
            "cores" -> JsNumber(cores),
            "ram" -> JsNumber(ram),
            "outdirSize" -> JsNumber(outdirSize),
            "tmpdirSize" -> JsNumber(tmpdirSize)
        )
    )
  }
}

object Runtime {
  lazy val empty: Runtime = Runtime(OpaqueValue, OpaqueValue, 0, 0, 0, 0)

  /**
    * The total memory size.
    * @note this is annotated `nowarn` because it uses a function (getTotalPhysicalMemorySize)
    *       that is deprecated in JDK11 but not replaced until JDK14
    * @return total memory size in bytes
    */
  @nowarn
  private def totalMemorySize: Long = {
    val mbean = ManagementFactory.getOperatingSystemMXBean
      .asInstanceOf[com.sun.management.OperatingSystemMXBean]
    mbean.getTotalPhysicalMemorySize
  }

  private lazy val defaultTmpdir = {
    Paths.get(doPrivileged(new GetPropertyAction("java.io.tmpdir")))
  }

  def create(outdir: Path = Paths.get(""),
             tmpdir: Path = defaultTmpdir,
             minCores: Option[Int] = None,
             minRam: Option[Long] = None,
             minOutdirSize: Option[Long] = None,
             minTmpdirSize: Option[Long] = None): Runtime = {
    val cores = JavaRuntime.getRuntime.availableProcessors()
    if (minCores.exists(_ > cores)) {
      throw new Exception(s"avaiable cores ${cores} is less than min cores ${minCores}")
    }
    val ram = totalMemorySize
    if (minRam.exists(_ > ram)) {
      throw new Exception(s"avaiable ram ${ram} is less than min ram ${minRam}")
    }
    val outdirSize = outdir.getRoot.toFile.getFreeSpace
    if (minOutdirSize.exists(_ > outdirSize)) {
      throw new Exception(
          s"avaiable outdir size ${outdirSize} is less than min outdir size ${minOutdirSize}"
      )
    }
    val tmpdirSize = tmpdir.getRoot.toFile.getFreeSpace
    if (minTmpdirSize.exists(_ > tmpdirSize)) {
      throw new Exception(
          s"avaiable tmpdir size ${tmpdirSize} is less than min tmpdir size ${minTmpdirSize}"
      )
    }
    Runtime(outdir.toString, tmpdir.toString, cores, ram, outdirSize, tmpdirSize)
  }
}

/**
  * Context used when evaluating expressions
  * @param self the value of `self`, if any
  * @param inputs the input values
  * @param runtime [[Runtime]]
  */
case class EvaluatorContext(self: CwlValue = NullValue,
                            inputs: ObjectValue = ObjectValue.empty,
                            runtime: Runtime = Runtime.empty) {
  def toScope: Scope = {
    Scope.create(
        Map(
            "self" -> self.toJson,
            "inputs" -> inputs.toJson,
            "runtime" -> runtime.toJson
        )
    )
  }

  def contains(name: String): Boolean = {
    Set("self", "inputs", "runtime").contains(name)
  }

  def apply(name: String): CwlValue = {
    name match {
      case "self"    => self
      case "inputs"  => inputs
      case "runtime" => runtime
      case _         => throw new Exception(s"context does not contain ${name}")
    }
  }
}

object EvaluatorContext {
  lazy val empty: EvaluatorContext = EvaluatorContext()

  /**
    * Creates an `inputs` map from all the inputs that contain a default value.
    * @param inputs all the process inputs
    * @return
    */
  def createStaticInputs(inputs: Vector[CommandInputParameter]): Map[String, CwlValue] = {
    inputs.collect {
      case param if param.id.isDefined && param.default.isDefined =>
        param.id.get.name.get -> param.default.get
    }.toMap
  }
}

/**
  * Performs interpolation of CWL strings, including the evaluation of
  * ECMAscript expressions and parameter references.
  *
  * If the `InlineJavascriptRequirement` is specified, then `jsEnabled=true`
  * and expressions are evaluated using the Rhino Javascript engine. Otherwise,
  * expressions are assumed to be parameter references and are parsed using a
  * simple ANTLR4-based parser.
  *
  * @param jsEnabled was the `InlineJavascriptRequirement` specified?
  * @param jsLibrary optional Javascript library to be included in the evaluation
  * @param schemaDefs schema defintions to use when resolving types
  * @param trace whether to print ANTLR parser trace information
  */
case class Evaluator(jsEnabled: Boolean = false,
                     jsLibrary: Option[String] = None,
                     schemaDefs: Map[String, CwlSchema] = Map.empty,
                     trace: Boolean = false) {
  private lazy val jsPreamble: String = jsLibrary.map(lib => s"${lib}\n").getOrElse("")

  def applyEcmaScript(script: String,
                      cwlTypes: Vector[CwlType],
                      ctx: EvaluatorContext): (CwlType, CwlValue) = {
    val engine = Engine(ctx.toScope)
    val result =
      try {
        engine.evalToJson(script).getOrElse(JsNull)
      } catch {
        case ex: Throwable =>
          throw new Exception(s"could not evaluate ECMA script ${script}", ex)
      }
    CwlValue.deserialize(result, cwlTypes, schemaDefs)
  }

  private def checkCoercibleTo(value: CwlValue, cwlTypes: Vector[CwlType]): (CwlType, CwlValue) = {
    val firstType = cwlTypes
      .collectFirst {
        case t if value.coercibleTo(t) => t
      }
      .getOrElse(
          throw new Exception(s"${value} is not coercible to any of ${cwlTypes}")
      )
    (firstType, value)
  }

  def applyEcmaString(ecmaString: EcmaString,
                      cwlTypes: Vector[CwlType],
                      ctx: EvaluatorContext): (CwlType, CwlValue) = {
    ecmaString match {
      case StringLiteral(s) =>
        checkCoercibleTo(StringValue(s), cwlTypes)
      case CompoundString(parts) =>
        checkCoercibleTo(
            StringValue(parts.map(applyEcmaString(_, cwlTypes, ctx).toString).mkString("")),
            cwlTypes
        )
      case EcmaExpr(expr) =>
        val script = s"${jsPreamble}${expr};"
        applyEcmaScript(script, cwlTypes, ctx)
      case EcmaFunctionBody(body) =>
        val script = s"""${jsPreamble}function __anon__() {
                        |  ${body};
                        |}
                        |__anon__();""".stripMargin
        applyEcmaScript(script, cwlTypes, ctx)
    }
  }

  def applyEcmaString(ecmaString: EcmaString,
                      cwlType: CwlType,
                      ctx: EvaluatorContext): (CwlType, CwlValue) = {
    applyEcmaString(ecmaString, Vector(cwlType), ctx)
  }

  def applyParameterValue(expr: ParameterValue,
                          cwlTypes: Vector[CwlType],
                          ctx: EvaluatorContext): (CwlType, CwlValue) = {
    checkCoercibleTo(
        expr match {
          case LiteralValue(value) => value
          case CompoundValue(parts) =>
            StringValue(
                parts
                  .map(applyParameterValue(_, Vector(CwlString, CwlNull), ctx).toString)
                  .mkString("")
            )
          case ParameterReference(rootSymbol, segments) =>
            rootSymbol.value match {
              case symbol if ctx.contains(symbol) =>
                segments.foldLeft(ctx(rootSymbol.value)) {
                  case (value, segment) =>
                    (value, segment) match {
                      case (NullValue, _) =>
                        throw new Exception(
                            s"cannot evaluate right-hand side ${segment} for left-hand side null"
                        )
                      case (value: StringIndexable, Symbol(index)) =>
                        value(index)
                      case (value: StringIndexable, StringIndex(index)) =>
                        value(index)
                      case (value: IntIndexable, IntIndex(index)) =>
                        value(index)
                      case (value: IntIndexable, Symbol("length")) =>
                        IntValue(value.length)
                      case _ =>
                        throw new Exception(s"cannot evaluate ${value}${segment}")
                    }
                }
              case "null" => NullValue
              case other =>
                throw new Exception(s"symbol ${other} not found in context")
            }
        },
        cwlTypes
    )
  }

  def applyParameterValue(expr: ParameterValue,
                          cwlType: CwlType,
                          ctx: EvaluatorContext): CwlValue = {
    applyParameterValue(expr, Vector(cwlType), ctx)._2
  }

  /**
    * The evaluator to use - simple parameter reference evaluator or Rhino
    * Javascript evaluator, depending on the value of `jsEnabled`.
    */
  private lazy val eval: (String, Vector[CwlType], EvaluatorContext) => (CwlType, CwlValue) = {
    if (jsEnabled) {
      val parser = EcmaStringParser(trace)
      (s: String, cwlTypes: Vector[CwlType], ctx: EvaluatorContext) =>
        applyEcmaString(parser(s), cwlTypes, ctx)
    } else {
      val parser = ParameterReferenceParser(trace)
      (s: String, cwlTypes: Vector[CwlType], ctx: EvaluatorContext) =>
        applyParameterValue(parser(s), cwlTypes, ctx)
    }
  }

  def apply(s: String, cwlTypes: Vector[CwlType], ctx: EvaluatorContext): (CwlType, CwlValue) = {
    if (!s.contains('$')) {
      // if an value does not contain a '$', there are no expressions to evaluate
      checkCoercibleTo(StringValue(s), cwlTypes)
    } else {
      eval(s, cwlTypes, ctx)
    }
  }

  def apply(s: String, cwlType: CwlType, ctx: EvaluatorContext): CwlValue = {
    apply(s, Vector(cwlType), ctx)._2
  }

  /**
    * Evaluates a string whose result must be coercible to a string.
    * @param s the string to evaluate
    * @param ctx [[EvaluatorContext]]
    * @return the result string value
    */
  def applyString(s: String, ctx: EvaluatorContext = EvaluatorContext.empty): String = {
    apply(s, CwlString, ctx) match {
      case StringValue(value) => value
      case _                  => throw new Exception("expected string")
    }
  }

  def evaluate(value: CwlValue,
               cwlTypes: Vector[CwlType],
               ctx: EvaluatorContext): (CwlType, CwlValue) = {
    def inner(innerValue: CwlValue, innerTypes: Vector[CwlType]): (CwlType, CwlValue) = {
      innerValue match {
        case StringValue(s) => apply(s, innerTypes, ctx)
        case ArrayValue(items) =>
          innerTypes.iterator
            .map {
              case arrayType: CwlArray =>
                try {
                  val (types, values) = items.map(inner(_, arrayType.itemTypes)).unzip
                  Some((CwlArray(types.distinct), ArrayValue(values)))
                } catch {
                  case _: Throwable => None
                }
              case _ => None
            }
            .collectFirst {
              case Some(value) => value
            }
            .getOrElse(
                throw new Exception(
                    s"array ${items} does not evaluate to any of ${innerTypes}"
                )
            )
        case ObjectValue(members) =>
          innerTypes.iterator
            .map {
              case record: CwlInputRecord =>
                try {
                  val (types, values) = members.map {
                    case (key, value) =>
                      val (t, v) = inner(value, record.fields(key).types)
                      (key -> t, key -> v)
                  }.unzip
                  val recordType = CwlInputRecord(types.map {
                    case (name, t) => name -> CwlInputRecordField(name, Vector(t))
                  }.toMap)
                  Some((recordType, ObjectValue(values.toMap)))
                } catch {
                  case _: Throwable => None
                }
              case _ => None
            }
            .collectFirst {
              case Some(value) => value
            }
            .getOrElse(
                throw new Exception(
                    s"object ${members} does not evaluate to any of ${innerTypes}"
                )
            )
        case _ => value.coerceTo(cwlTypes)
      }
    }
    inner(value, cwlTypes)
  }

  def evaluateMap(
      map: Map[String, (CwlType, CwlValue)],
      ctx: EvaluatorContext
  ): Map[String, (CwlType, CwlValue)] = {
    map.foldLeft(Map.empty[String, (CwlType, CwlValue)]) {
      case (accu, (key, (cwlType, cwlValue))) =>
        accu + (key -> evaluate(cwlValue, Vector(cwlType), ctx))
    }
  }
}

object Evaluator {
  lazy val default: Evaluator = Evaluator()

  def create(requirements: Vector[Requirement]): Evaluator = {
    val (jsEnabled, jsLibrary) = RequirementUtils.getJsRequirements(requirements)
    val schemaDefs = RequirementUtils.getSchemaDefs(requirements)
    Evaluator(jsEnabled, jsLibrary, schemaDefs)
  }
}
