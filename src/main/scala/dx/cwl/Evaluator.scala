package dx.cwl

import java.lang.management.ManagementFactory
import java.nio.ByteBuffer
import java.nio.file.{Path, Paths}
import java.lang.{Runtime => JavaRuntime}
import java.security.AccessController.doPrivileged
import dx.js.{Engine, Scope}
import dx.util.{
  AddressableFileSource,
  FileNode,
  FileSource,
  FileSourceResolver,
  FileUtils,
  LocalFileSource
}
import org.antlr.v4.runtime.{CodePointBuffer, CodePointCharStream, CommonTokenStream}
import org.commonwl.cwl.ecma.v1_2.{CwlEcmaStringLexer, CwlEcmaStringParser}
import org.commonwl.cwl.refparser.v1_2.{CwlParameterReferenceLexer, CwlParameterReferenceParser}
import spray.json._
import sun.security.action.GetPropertyAction

import java.util.UUID
import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.{SeqMap, TreeSeqMap}
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

  override def fields: SeqMap[String, CwlValue] = {
    keys.map(key => key -> apply(key)).to(TreeSeqMap)
  }

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
             maxCores: Option[Int] = None,
             minRam: Option[Long] = None,
             maxRam: Option[Long] = None,
             minOutdirSize: Option[Long] = None,
             maxOutdirSize: Option[Long] = None,
             minTmpdirSize: Option[Long] = None,
             maxTmpdirSize: Option[Long] = None): Runtime = {
    val actualCores = JavaRuntime.getRuntime.availableProcessors()
    if (minCores.exists(_ > actualCores)) {
      throw new Exception(s"avaiable cores ${actualCores} is less than min cores ${minCores}")
    }
    val cores = maxCores.map(Math.min(_, actualCores)).getOrElse(actualCores)
    val actualRam = totalMemorySize
    if (minRam.exists(_ > actualRam)) {
      throw new Exception(s"avaiable ram ${actualRam} is less than min ram ${minRam}")
    }
    val ram = maxRam.map(Math.min(_, actualRam)).getOrElse(actualRam)
    val actualOutdirSize = outdir.getRoot.toFile.getFreeSpace
    if (minOutdirSize.exists(_ > actualOutdirSize)) {
      throw new Exception(
          s"avaiable outdir size ${actualOutdirSize} is less than min outdir size ${minOutdirSize}"
      )
    }
    val outdirSize = maxOutdirSize.map(Math.min(_, actualOutdirSize)).getOrElse(actualOutdirSize)
    val actualTmpdirSize = tmpdir.getRoot.toFile.getFreeSpace
    if (minTmpdirSize.exists(_ > actualTmpdirSize)) {
      throw new Exception(
          s"avaiable tmpdir size ${actualTmpdirSize} is less than min tmpdir size ${minTmpdirSize}"
      )
    }
    val tmpdirSize = maxTmpdirSize.map(Math.min(_, actualTmpdirSize)).getOrElse(actualTmpdirSize)
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
  val MaxContentsSize: Long = 64 * 1024
  private lazy val nameRegexp = "(.*)(\\..*)".r

  /**
    * Apply "implementation must" rules from the spec.
    */
  def finalizeInputValue(value: CwlValue,
                         cwlType: CwlType,
                         param: Identifiable with Loadable,
                         inputDir: Path,
                         fileResolver: FileSourceResolver = FileSourceResolver.get,
                         ignoreMissingRequired: Boolean = false): CwlValue = {
    def finalizeFileSources(fileSources: Vector[FileSource],
                            recurseListings: Boolean): Vector[PathValue] = {
      fileSources.map {
        case fs: AddressableFileSource if fs.isDirectory =>
          val dirValue = DirectoryValue(location = Some(fs.address))
          finalizePathWithFileSource(dirValue, Some(fs), recurseListings)
        case fs: AddressableFileSource =>
          val fileValue = FileValue(location = Some(fs.address))
          finalizePathWithFileSource(fileValue, Some(fs))
        case other =>
          throw new Exception(s"cannot finalize FileSource ${other}")
      }
    }

    def finalizePathWithFileSource(pathValue: PathValue,
                                   fileSource: Option[AddressableFileSource],
                                   recurseListings: Boolean = true): PathValue = {
      lazy val isDirectory = Some(PathValue.isDirectory(pathValue))
      val (newFileSource, newLocation, newPath) = fileSource match {
        case Some(local: LocalFileSource) if local.canonicalPath.toFile.exists() =>
          (local, local.uri, local.canonicalPath)
        case Some(local: LocalFileSource) =>
          val newPath = inputDir.resolve(local.name)
          (fileResolver.fromPath(newPath, isDirectory = isDirectory), newPath.toUri, newPath)
        case Some(fs) =>
          fs.uri match {
            case uri if uri.getScheme != null =>
              (fs, uri, inputDir.resolve(fs.name))
            case uri =>
              val newPath = Paths.get(uri.getPath) match {
                case p if p.toFile.exists() => p.toRealPath()
                case p                      => inputDir.resolve(p.getFileName)
              }
              (fs, newPath.toUri, newPath)
          }
        case None if pathValue.path.nonEmpty =>
          val newPath = Paths.get(pathValue.path.get) match {
            case p if p.toFile.exists() => p.toRealPath()
            case p                      => inputDir.resolve(p.getFileName)
          }
          (fileResolver.fromPath(newPath, isDirectory = isDirectory), newPath.toUri, newPath)
        case None =>
          val randPath = Iterator
            .continually(UUID.randomUUID().toString)
            .map(inputDir.resolve)
            .collectFirst {
              case p if !p.toFile.exists() => p.toAbsolutePath
            }
            .get
          (fileResolver.fromPath(randPath, isDirectory = isDirectory), randPath.toUri, randPath)
      }
      val newBasename = pathValue.basename match {
        case Some(basename) => basename
        case None           => newPath.getFileName.toString
      }
      pathValue match {
        case f: FileValue =>
          val (nameRoot, nameExt) = newBasename match {
            case nameRegexp(root, ext) => (root, ext)
            case _                     => (newBasename, "")
          }
          val dirname = Option(newPath.getParent).getOrElse(inputDir)
          val newChecksum = f.checksum // TODO
          val fileExists = newPath.toFile.exists()
          val newSize = (f.size, newFileSource) match {
            case (Some(size), _)         => Some(size)
            case (None, _) if fileExists => Some(newPath.toFile.length())
            case (None, f: FileNode) =>
              try {
                Some(f.size)
              } catch {
                case _: Throwable => None
              }
            case _ => None
          }
          val newSecondaryFiles = f.secondaryFiles.map(finalizePath)
          val newContents = if (param.loadContents && f.contents.isEmpty && fileExists) {
            Some(FileUtils.readFileContent(newPath, maxSize = Some(MaxContentsSize)))
          } else {
            f.contents
          }
          FileValue(
              Some(newLocation.toString),
              Some(newPath.toString),
              Some(newBasename),
              Some(dirname.toString),
              Some(nameRoot),
              Some(nameExt),
              newChecksum,
              newSize,
              newSecondaryFiles,
              f.format,
              newContents
          )
        case d: DirectoryValue =>
          val newListing = if (d.listing.isEmpty && recurseListings) {
            param.loadListing match {
              case LoadListing.Shallow =>
                finalizeFileSources(newFileSource.listing(recursive = false),
                                    recurseListings = false)
              case LoadListing.Deep =>
                finalizeFileSources(newFileSource.listing(recursive = true), recurseListings = true)
              case _ => d.listing
            }
          } else {
            d.listing.map(finalizePath)
          }
          DirectoryValue(
              Some(newLocation.toASCIIString),
              Some(newPath.toString),
              Some(newBasename),
              newListing
          )
      }
    }

    def finalizePath(pathValue: PathValue): PathValue = {
      val location = pathValue.location.orElse(pathValue.path)
      val fileSource = (location, pathValue) match {
        case (Some(uri), _: FileValue)      => Some(fileResolver.resolve(uri))
        case (Some(uri), _: DirectoryValue) => Some(fileResolver.resolveDirectory(uri))
        case _                              => None
      }
      finalizePathWithFileSource(pathValue, fileSource)
    }

    (cwlType, value) match {
      case (t, NullValue) if CwlOptional.isOptional(t) || ignoreMissingRequired =>
        NullValue
      case (_, NullValue) =>
        throw new Exception(s"missing required input ${param.frag}")
      case (CwlFile, f: FileValue)           => finalizePath(f)
      case (CwlDirectory, d: DirectoryValue) => finalizePath(d)
      case _                                 => value
    }
  }

  /**
    * Creates an `inputs` map from the given input values.
    * @param inputs all the process inputs
    * @param inputDir the directory in which to create files/directories that
    *                 have no location - defaults to the current directory
    * @return
    */
  def createInputs(inputs: Map[Identifiable with Loadable, (CwlType, CwlValue)],
                   inputDir: Path = Paths.get("."),
                   fileResolver: FileSourceResolver = FileSourceResolver.get): ObjectValue = {
    ObjectValue(
        inputs
          .map {
            case (param, (t, v)) =>
              // inputs are evaluated before valueFrom, so the value of a required
              // parameter may be null at this point
              param.name -> finalizeInputValue(
                  v,
                  t,
                  param,
                  inputDir,
                  fileResolver,
                  ignoreMissingRequired = true
              )
          }
          .to(TreeSeqMap)
    )
  }

  def inputsFromParameters(
      inputs: Map[InputParameter, CwlValue],
      inputDir: Path = Paths.get("."),
      fileResolver: FileSourceResolver = FileSourceResolver.get
  ): ObjectValue = {
    val i: Map[Identifiable with Loadable, (CwlType, CwlValue)] = inputs.map {
      case (param, value) => param -> (param.cwlType, value)
    }
    createInputs(i, inputDir, fileResolver)
  }

  /**
    * Creates an `inputs` map from all the inputs that contain a default value.
    * @param inputs all the process inputs
    * @return
    */
  def inputsFromDefaults(inputs: Vector[InputParameter],
                         inputDir: Path = Paths.get(".")): ObjectValue = {
    ObjectValue(
        inputs
          .collect {
            case param if param.id.isDefined && param.default.isDefined =>
              param.id.get.frag.get -> finalizeInputValue(param.default.get,
                                                          param.cwlType,
                                                          param,
                                                          inputDir)
          }
          .to(TreeSeqMap)
    )
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
                      cwlType: CwlType,
                      ctx: EvaluatorContext): (CwlType, CwlValue) = {
    val engine = Engine(ctx.toScope)
    val result =
      try {
        engine.evalToJson(script).getOrElse(JsNull)
      } catch {
        case ex: Throwable =>
          throw new Exception(s"could not evaluate ECMA script ${script}", ex)
      }
    CwlValue.deserialize(result, cwlType, schemaDefs)
  }

  @tailrec
  private def checkCoercibleTo(value: CwlValue, cwlType: CwlType): (CwlType, CwlValue) = {
    (cwlType, value) match {
      case (CwlNull, NullValue)                              => (cwlType, value)
      case (_, NullValue) if CwlOptional.isOptional(cwlType) => (cwlType, value)
      case (_, NullValue) =>
        throw new Exception(s"null is not coercible to non-optional type ${cwlType}")
      case (CwlOptional(t), _) => checkCoercibleTo(value, t)
      case (CwlMulti(types), _) =>
        types
          .collectFirst {
            case t if value.coercibleTo(t) => (t, value)
          }
          .getOrElse(
              throw new Exception(s"${value} is not coercible to any of ${types}")
          )
      case _ if value.coercibleTo(cwlType) => (cwlType, value)
      case _ =>
        throw new Exception(s"${value} is not coercible to ${cwlType}")
    }
  }

  def applyEcmaString(ecmaString: EcmaString,
                      cwlType: CwlType,
                      ctx: EvaluatorContext): (CwlType, CwlValue) = {
    ecmaString match {
      case StringLiteral(s) =>
        checkCoercibleTo(StringValue(s), cwlType)
      case CompoundString(parts) =>
        checkCoercibleTo(
            StringValue(parts.map(applyEcmaString(_, cwlType, ctx)._2.toString).mkString("")),
            cwlType
        )
      case EcmaExpr(expr) =>
        val script = s"${jsPreamble}${expr};"
        applyEcmaScript(script, cwlType, ctx)
      case EcmaFunctionBody(body) =>
        val script = s"""${jsPreamble}function __anon__() {
                        |  ${body};
                        |}
                        |__anon__();""".stripMargin
        applyEcmaScript(script, cwlType, ctx)
    }
  }

  def applyParameterValue(expr: ParameterValue,
                          cwlType: CwlType,
                          ctx: EvaluatorContext): (CwlType, CwlValue) = {
    checkCoercibleTo(
        expr match {
          case LiteralValue(value) => value
          case CompoundValue(parts) =>
            StringValue(
                parts
                  .map(applyParameterValue(_, CwlOptional(CwlString), ctx)._2.toString)
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
                            s"cannot evaluate right-hand side ${segment} for left-hand side null in ${expr}"
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
                        throw new Exception(s"cannot evaluate ${value}${segment} in ${expr}")
                    }
                }
              case "null" => NullValue
              case other =>
                throw new Exception(s"symbol ${other} not found in context")
            }
        },
        cwlType
    )
  }

  /**
    * The evaluator to use - simple parameter reference evaluator or Rhino
    * Javascript evaluator, depending on the value of `jsEnabled`.
    */
  private lazy val eval: (String, CwlType, EvaluatorContext) => (CwlType, CwlValue) = {
    if (jsEnabled) {
      val parser = EcmaStringParser(trace)
      (s: String, cwlType: CwlType, ctx: EvaluatorContext) =>
        applyEcmaString(parser(s), cwlType, ctx)
    } else {
      val parser = ParameterReferenceParser(trace)
      (s: String, cwlType: CwlType, ctx: EvaluatorContext) =>
        applyParameterValue(parser(s), cwlType, ctx)
    }
  }

  def apply(s: String, cwlType: CwlType, ctx: EvaluatorContext): (CwlType, CwlValue) = {
    if (!s.contains('$')) {
      // if an value does not contain a '$', there are no expressions to evaluate
      checkCoercibleTo(StringValue(s), cwlType)
    } else {
      eval(s, cwlType, ctx)
    }
  }

  /**
    * Evaluates a string whose result must be coercible to a string.
    * @param s the string to evaluate
    * @param ctx [[EvaluatorContext]]
    * @return the result string value
    */
  def applyString(s: String, ctx: EvaluatorContext = EvaluatorContext.empty): String = {
    apply(s, CwlString, ctx)._2 match {
      case StringValue(value) => value
      case _                  => throw new Exception("expected string")
    }
  }

  def evaluate(value: CwlValue, cwlType: CwlType, ctx: EvaluatorContext): (CwlType, CwlValue) = {
    def evaluateObject(obj: ObjectValue,
                       fields: Map[String, CwlRecordField]): (Map[String, CwlType], ObjectValue) = {
      val (types, values) = obj.fields.map {
        case (key, value) =>
          val (t, v) = inner(value, fields(key).cwlType)
          (key -> t, key -> v)
      }.unzip
      (types.toMap, ObjectValue(values.to(TreeSeqMap)))
    }
    def inner(innerValue: CwlValue, innerType: CwlType): (CwlType, CwlValue) = {
      (innerType, innerValue) match {
        case (_, StringValue(s)) => apply(s, innerType, ctx)
        case (arrayType: CwlArray, array: ArrayValue) =>
          val (types, values) = array.items.map(i => inner(i, arrayType.itemType)).unzip
          (CwlArray(CwlType.flatten(types.distinct)), ArrayValue(values))
        case (recordType: CwlInputRecord, obj: ObjectValue) =>
          val (types, value) = evaluateObject(obj, recordType.fields)
          val fields = types
            .map {
              case (name, t) => name -> CwlInputRecordField(name, t)
            }
            .to(TreeSeqMap)
          (CwlInputRecord(fields), value)
        case (recordType: CwlOutputRecord, obj: ObjectValue) =>
          val (types, value) = evaluateObject(obj, recordType.fields)
          val fields = types
            .map {
              case (name, t) => name -> CwlOutputRecordField(name, t)
            }
            .to(TreeSeqMap)
          (CwlOutputRecord(fields), value)
        case (CwlMulti(types), _) =>
          types.iterator
            .map {
              case CwlAny => None
              case t =>
                try {
                  Some(inner(innerValue, t))
                } catch {
                  case _: Throwable => None
                }
            }
            .collectFirst {
              case Some(value) => value
            }
            .getOrElse(
                if (types.contains(CwlAny)) {
                  (CwlAny, innerValue)
                } else {
                  throw new Exception(
                      s"${innerValue} does not evaluate to any of ${types}"
                  )
                }
            )
        case _ => (innerType, innerValue.coerceTo(innerType))
      }
    }
    inner(value, cwlType)
  }

  def evaluateMap(
      map: Map[String, (CwlType, CwlValue)],
      ctx: EvaluatorContext
  ): Map[String, (CwlType, CwlValue)] = {
    map.foldLeft(Map.empty[String, (CwlType, CwlValue)]) {
      case (accu, (key, (cwlType, cwlValue))) =>
        accu + (key -> evaluate(cwlValue, cwlType, ctx))
    }
  }
}

object Evaluator {
  lazy val default: Evaluator = Evaluator()

  /**
    * Create Evaluator from JavaScript and SchemaDef requirements/hints given.
    * @param requirements Vector of Requirements in increasing order of priority.
    *                     If multiple JavaScript requirements are given, the last
    *                     one takes priority. If multiple SchemaDef requirements are
    *                     given, they are merged with later definitions overriding
    *                     earlier ones.
    * @param hints Vector of Hints in increasing order of priority.
    * @return
    */
  def create(requirements: Vector[Requirement], hints: Vector[Hint]): Evaluator = {
    val (jsEnabled, jsLibrary) = HintUtils.getJsHint(requirements) match {
      case (true, lib) => (true, lib)
      case (false, _)  => HintUtils.getJsHint(hints)
    }
    val schemaDefs = HintUtils.getSchemaDefs(requirements)
    Evaluator(jsEnabled, jsLibrary, schemaDefs)
  }
}
