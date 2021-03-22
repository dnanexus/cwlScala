package dx.cwl

import dx.cwl.Document.{Document, DocumentAdder}

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}
import java.net.URI
import java.nio.file.{Path, Paths}
import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, ExpressionToolImpl, OperationImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}

import scala.jdk.CollectionConverters._

object Parser {
  lazy val default: Parser = Parser()

  def create(baseUri: Option[URI] = None,
             loadingOptions: Option[LoadingOptions] = None,
             schemaDefs: Vector[CwlSchema] = Vector.empty,
             hintSchemas: Vector[HintSchema] = Vector.empty): Parser = {
    val schemaDefMap = schemaDefs.collect {
      case schema if schema.hasName => schema.frag -> schema
    }.toMap
    val hintSchemaMap = hintSchemas.map(s => s.className -> s).toMap
    Parser(baseUri, loadingOptions, schemaDefMap, hintSchemaMap)
  }
}

case class Parser(baseUri: Option[URI] = None,
                  loadingOptions: Option[LoadingOptions] = None,
                  schemaDefs: Map[String, CwlSchema] = Map.empty,
                  hintSchemas: Map[String, HintSchema] = Map.empty) {
  private var cache: Map[Path, (Process, Map[String, Process])] = Map.empty
  private lazy val normalizedBaseUri = baseUri.map(Utils.normalizeUri).orNull

  def detectVersionAndClass(inputStream: InputStream): Option[(String, String)] = {
    try {
      val yamlLoader = new Load(LoadSettings.builder().build())
      val doc =
        yamlLoader.loadFromInputStream(inputStream).asInstanceOf[java.util.Map[String, Any]]
      if (doc.containsKey("cwlVersion")) {
        val version = doc.get("cwlVersion").asInstanceOf[String]
        if (version.startsWith("v1.2")) {
          Some(version, doc.get("class").asInstanceOf[String])
        } else {
          None
        }
      } else {
        None
      }
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Can a file be parsed as CWL?
    */
  def detectVersionAndClass(path: Path): Option[(String, String)] = {
    detectVersionAndClass(new FileInputStream(path.toFile))
  }

  def detectVersionAndClass(sourceCode: String): Option[(String, String)] = {
    detectVersionAndClass(new ByteArrayInputStream(sourceCode.getBytes()))
  }

  def parse(
      doc: java.lang.Object,
      source: Option[Path] = None,
      defaultFrag: Option[String] = None,
      dependencies: Document = Document.empty,
      rawProcesses: Map[String, java.lang.Object] = Map.empty,
      isGraph: Boolean = false
  ): (Process, Map[String, Process]) = {
    doc match {
      case tool: CommandLineToolImpl =>
        val proc = CommandLineTool(tool, this, source, defaultFrag, isGraph)
        (proc, dependencies.addProcess(proc))
      case expressionTool: ExpressionToolImpl =>
        val proc = ExpressionTool(expressionTool, this, source, defaultFrag, isGraph)
        (proc, dependencies.addProcess(proc))
      case operation: OperationImpl =>
        val proc = Operation(operation, this, source, defaultFrag, isGraph)
        (proc, dependencies.addProcess(proc))
      case workflow: WorkflowImpl =>
        Workflow.parse(workflow, this, source, defaultFrag, dependencies, rawProcesses, isGraph)
      case graph: java.util.List[_] =>
        // this is the result of parsing a packed workflow - the
        // top-level process will be called "main".
        val rawProcesses = graph.asScala.toVector.map {
          case tool: CommandLineToolImpl =>
            Identifier.parse(tool.getId.get()).frag.get -> tool
          case workflow: WorkflowImpl =>
            Identifier.parse(workflow.getId.get()).frag.get -> workflow
          case expressionTool: ExpressionToolImpl =>
            Identifier.parse(expressionTool.getId.get()).frag.get -> expressionTool
          case operation: OperationImpl =>
            Identifier.parse(operation.getId.get()).frag.get -> operation
          case other =>
            throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
        }.toMap
        val result = rawProcesses.foldLeft(dependencies) {
          case (accu, (name, _)) if accu.contains(name) => accu
          case (accu, (_, rawProc)) =>
            val (_, newAccu) =
              parse(rawProc, dependencies = accu, rawProcesses = rawProcesses, isGraph = true)
            newAccu
        }
        (result("main"), result)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
    }
  }

  /**
    * Parses a CWL document from a file.
    * @note currently, only CommandLineTool documents are supported.
    * @param path path to the document
    * @param defaultFrag tool/workflow name, in case it is not specified in the document
    *             if not specified, the name of the file without .cwl is used
    * @return a [[Process]]
    */
  def parseFile(path: Path, defaultFrag: Option[String] = None): (Process, Map[String, Process]) = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          Some(path),
          defaultFrag
      )
      cache = cache + (path -> result)
      result
    }
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document
    * @param defaultFrag tool/workflow name, in case it is not specified in the document
    * @return a [[Process]]
    */
  def parseString(sourceCode: String,
                  defaultFrag: Option[String] = None): (Process, Map[String, Process]) = {
    parse(RootLoader.loadDocument(sourceCode, normalizedBaseUri, loadingOptions.orNull),
          None,
          defaultFrag)
  }

  def parseImport(relPath: String): (Process, Map[String, Process]) = {
    val path = baseUri.map(uri => Paths.get(uri.resolve(relPath))).getOrElse(Paths.get(relPath))
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          Some(path),
          None
      )
      cache = cache + (path -> result)
      result
    }
  }
}
