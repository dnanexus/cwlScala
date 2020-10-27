package dx.cwl

import java.nio.ByteBuffer

import dx.cwl.CwlValue.StringValue
import org.antlr.v4.runtime.{CodePointBuffer, CodePointCharStream, CommonTokenStream}
import org.commonwl.cwl.stringparser.v1_2.{
  CwlStringLexer,
  CwlStringParser,
  CwlStringParserBaseVisitor
}
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
case class CompoundString(parts: Vector[CwlExpr]) extends CwlExpr

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

class CwlStringParserVisitorImpl extends CwlStringParserBaseVisitor[CwlExpr] {

  private def visitStringIndexPart(ctx: CwlStringParser.String_index_partContext): String = {
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
      ctx: CwlStringParser.Expr_segmentContext
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

  override def visitInterpolated_string(
      ctx: CwlStringParser.Interpolated_stringContext
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
      CompoundString(allParts)
    } else {
      CwlValueExpr(StringValue(allParts.map(_.toString).mkString("")))
    }
  }
}

object ParameterReferenceParser {
  private lazy val visitor = new CwlStringParserVisitorImpl
  private val trace: Boolean = false

  def apply(s: String): CwlExpr = {
    // the string might contain an expression
    val codePointBuffer: CodePointBuffer =
      CodePointBuffer.withBytes(ByteBuffer.wrap(s.getBytes()))
    val charStream = CodePointCharStream.fromBuffer(codePointBuffer)
    val lexer = new CwlStringLexer(charStream)
    val parser = new CwlStringParser(new CommonTokenStream(lexer))
    parser.setTrace(trace)
    try {
      visitor.visit(parser.interpolated_string())
    } catch {
      case ex: Throwable =>
        throw new Exception(s"error evaluating string ${s}", ex)
    }
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
case class Evaluator(jsEnabled: Boolean = false, jsLibrary: Option[String] = None) {
  if (jsEnabled) {
    throw new Exception("Javascript evaluation is not yet supported")
  }

  def apply(s: String): CwlExpr = {
    // if an expression does not contain a '$', there are no
    // expressions to evaluate
    if (!s.contains('$')) {
      return CwlValueExpr(StringValue(s))
    }
    if (jsEnabled) {
      throw new Exception("Javascript evaluation is not yet supported")
    } else {
      ParameterReferenceParser.apply(s)
    }
  }
}

object Evaluator {
  lazy val default: Evaluator = Evaluator()
}
