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

sealed trait CwlExpr

case class CwlValueExpr(value: CwlValue) extends CwlExpr {
  override def toString: String = {
    value.toString
  }
}

/**
  * A string that includes placeholders.
  * @param parts the parts of the string - alternating `StringValue` and `CwlExpr` objects.
  * @example
  * "my name is ${name}" -> CompoundString(StringValue("my name is "), JavascriptExpr("name"))
  */
case class CompoundExpr(parts: Vector[CwlExpr]) extends CwlExpr

sealed trait ParameterReferencePart
case class Symbol(value: String) extends ParameterReferencePart
case class IntIndex(value: Int) extends ParameterReferencePart
case class StringIndex(value: String) extends ParameterReferencePart

/**
  * A parameter reference.
  * @param rootSymbol the initial symbol of the reference
  * @param segments additional parts of the reference
  */
case class ParameterReference(rootSymbol: Symbol, segments: Vector[ParameterReferencePart])
    extends CwlExpr

object ParameterReferenceParser {
  private val trace: Boolean = false

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
  ): ParameterReferencePart = {
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
  ): CwlExpr = {
    if (ctx.interpolated_string_part() == null) {
      throw new Exception("empty value")
    }
    val (parts, buf, hasExpr) = ctx
      .interpolated_string_part()
      .asScala
      .toVector
      .foldLeft((Vector.empty[CwlExpr], new StringBuilder, false)) {
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
              val stringVal = CwlValueExpr(StringValue(buf.toString))
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
      parts :+ CwlValueExpr(StringValue(buf.toString))
    } else {
      parts
    }
    if (parts.size == 1) {
      allParts.head
    } else if (hasExpr) {
      CompoundExpr(allParts)
    } else {
      CwlValueExpr(StringValue(allParts.map(_.toString).mkString("")))
    }
  }

  def apply(s: String): CwlExpr = {
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

trait EcmaString
case class StringLiteral(value: String) extends EcmaString
case class EcmaExpr(value: String) extends EcmaString
case class EcmaFunctionBody(value: String) extends EcmaString
case class CompoundString(parts: Vector[EcmaString]) extends EcmaString

/**
  * Parses a string that may contain ECMA expressions (`$(...)`) or function bodies
  * (`${...}`). The expressions themselves are not evaluated (which requires a
  * JavaScript engine).
  */
object EcmaStringParser {
  private val trace: Boolean = false

  private def visitExprParts(parts: Vector[CwlEcmaStringParser.Expr_partContext]): String = {
    parts
      .map { exprPart =>
        if (exprPart.EscPart() != null) {
          val escapedStr = exprPart.EscPart().toString
          if (!escapedStr.startsWith("\\")) {
            throw new Exception(s"invalid escaped string ${escapedStr}")
          }
          escapedStr.drop(1)
        } else if (exprPart.ExprPart() != null) {
          exprPart.ExprPart().toString
        } else {
          throw new Exception(s"invalid expr part ${exprPart}")
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
            if (stringPart.paren_expr() != null &&
                stringPart.paren_expr().expr() != null &&
                stringPart.paren_expr().expr().expr_part() != null) {
              val exprParts = stringPart
                .paren_expr()
                .expr()
                .expr_part()
                .asScala
                .toVector
              val expr = EcmaExpr(visitExprParts(exprParts))
              if (buf.isEmpty) {
                (accu :+ expr, buf, true)
              } else {
                val stringVal = StringLiteral(buf.toString)
                buf.clear()
                (accu ++ Vector(stringVal, expr), buf, true)
              }
            } else if (stringPart.brace_expr() != null &&
                       stringPart.brace_expr().expr() != null &&
                       stringPart.brace_expr().expr().expr_part() != null) {
              val exprParts = stringPart
                .brace_expr()
                .expr()
                .expr_part()
                .asScala
                .toVector
              val expr = EcmaFunctionBody(visitExprParts(exprParts))
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

case class Runtime(outdir: String,
                   tmpdir: String,
                   cores: Int,
                   ram: Long,
                   outdirSize: Long,
                   tmpdirSize: Long)
    extends ObjectLike {
  def toJson: JsValue = {
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

  override def coercibleTo(targetType: CwlType): Boolean = false
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

  private lazy val defaultTmpdir = Paths.get(doPrivileged(new GetPropertyAction("java.io.tmpdir")))

  def create(outdir: Path = Paths.get(""),
             tmpdir: Path = defaultTmpdir,
             minCores: Option[Int] = None,
             minRam: Option[Long] = None,
             minOutdirSize: Option[Long] = None,
             minTmpdirSize: Option[Long] = None): Runtime = {
    val cores = JavaRuntime.getRuntime.availableProcessors()
    if (minCores.forall(_ > cores)) {
      throw new Exception(s"avaiable cores ${cores} is less than min cores ${minCores}")
    }
    val ram = totalMemorySize
    if (minRam.forall(_ > ram)) {
      throw new Exception(s"avaiable ram ${ram} is less than min ram ${minRam}")
    }
    val outdirSize = outdir.getRoot.toFile.getFreeSpace
    if (minOutdirSize.forall(_ > outdirSize)) {
      throw new Exception(
          s"avaiable outdir size ${outdirSize} is less than min outdir size ${minOutdirSize}"
      )
    }
    val tmpdirSize = tmpdir.getRoot.toFile.getFreeSpace
    if (minTmpdirSize.forall(_ > tmpdirSize)) {
      throw new Exception(
          s"avaiable tmpdir size ${tmpdirSize} is less than min tmpdir size ${minTmpdirSize}"
      )
    }
    Runtime(outdir.toString, tmpdir.toString, cores, ram, outdirSize, tmpdirSize)
  }
}

case class EvaluatorContext(self: CwlValue = NullValue,
                            inputs: ObjectValue = ObjectValue.empty,
                            runtime: Runtime = Runtime.empty) {
  def toScope: Scope = {
    Scope.create(
        Map(
            "self" -> self.toJson,
            "input" -> inputs.toJson,
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

  def createStaticInputs(inputs: Vector[CommandInputParameter]): Map[String, CwlValue] = {
    inputs.collect {
      case param if param.id.isDefined && param.default.isDefined =>
        param.id.get -> param.default.get
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
  */
case class Evaluator(jsEnabled: Boolean = false,
                     jsLibrary: Option[String] = None,
                     schemaDefs: Map[String, CwlSchema] = Map.empty) {
  private lazy val jsPreamble: String = jsLibrary.map(lib => s"${lib}\n").getOrElse("")

  def applyEcmaScript(script: String,
                      cwlTypes: Vector[CwlType],
                      ctx: EvaluatorContext): CwlValue = {
    val engine = Engine(ctx.toScope)
    val result = engine.evalToJson(script).getOrElse(JsNull)
    CwlValue.deserialize(result, cwlTypes, schemaDefs)
  }

  def applyEcma(ecmaString: EcmaString,
                cwlTypes: Vector[CwlType],
                ctx: EvaluatorContext): CwlValue = {
    ecmaString match {
      case StringLiteral(s) =>
        StringValue(s)
      case CompoundString(parts) =>
        StringValue(parts.map(applyEcma(_, cwlTypes, ctx).toString).mkString(""))
      case EcmaExpr(expr) =>
        applyEcmaScript(s"${jsPreamble}${expr}", cwlTypes, ctx)
      case EcmaFunctionBody(body) =>
        val script = s"""${jsPreamble}function __anon__() {
                        |  ${body}
                        |}
                        |__anon__()""".stripMargin
        applyEcmaScript(script, cwlTypes, ctx)
    }
  }

  def applyEcma(ecmaString: EcmaString, cwlType: CwlType, ctx: EvaluatorContext): CwlValue = {
    applyEcma(ecmaString, Vector(cwlType), ctx)
  }

  def applyExpr(expr: CwlExpr, cwlTypes: Vector[CwlType], ctx: EvaluatorContext): CwlValue = {
    expr match {
      case CwlValueExpr(value) if value.coercibleTo(cwlTypes) =>
        value
      case CwlValueExpr(value) =>
        throw new Exception(s"${value} is not coercible to ${cwlTypes}")
      case CompoundExpr(parts) =>
        StringValue(parts.map(applyExpr(_, CwlString, ctx).toString).mkString(""))
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
    }
  }

  def applyExpr(expr: CwlExpr, cwlType: CwlType, ctx: EvaluatorContext): CwlValue = {
    applyExpr(expr, Vector(cwlType), ctx)
  }

  def apply(s: String, cwlTypes: Vector[CwlType], ctx: EvaluatorContext): CwlValue = {
    if (!s.contains('$')) {
      // if an expression does not contain a '$', there are no expressions to evaluate
      StringValue(s)
    } else if (jsEnabled) {
      applyEcma(EcmaStringParser.apply(s), cwlTypes, ctx)
    } else {
      applyExpr(ParameterReferenceParser.apply(s), cwlTypes, ctx)
    }
  }

  def apply(s: String, cwlType: CwlType, ctx: EvaluatorContext): CwlValue = {
    apply(s, Vector(cwlType), ctx)
  }
}

object Evaluator {
  lazy val default: Evaluator = Evaluator()
}
