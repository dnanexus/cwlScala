package dx.cwl

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
  private var cache: Map[Path, Document] = Map.empty
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
      name: Option[String] = None,
      dependencies: Document = Document.empty,
      rawProcesses: Map[String, java.lang.Object] = Map.empty,
      isGraph: Boolean = false
  ): (Process, Document) = {
    doc match {
      case tool: CommandLineToolImpl =>
        val proc = CommandLineTool(tool, this, source, name, isGraph)
        (proc, dependencies.add(proc, isPrimary = !isGraph))
      case expressionTool: ExpressionToolImpl =>
        val proc = ExpressionTool(expressionTool, this, source, name, isGraph)
        (proc, dependencies.add(proc, isPrimary = !isGraph))
      case operation: OperationImpl =>
        val proc = Operation(operation, this, source, name, isGraph)
        (proc, dependencies.add(proc, isPrimary = !isGraph))
      case workflow: WorkflowImpl =>
        Workflow.parse(workflow, this, source, name, dependencies, rawProcesses, isGraph)
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
        (result.primary, result)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
    }
  }

  /**
    * Parses a CWL document from a file.
    * @note currently, only CommandLineTool documents are supported.
    * @param path path to the document
    * @param name tool/workflow name, in case it is not specified in the document
    *             if not specified, the name of the file without .cwl is used
    * @return a [[Process]]
    */
  def parseFile(path: Path, name: Option[String] = None): (Process, Document) = {
    if (cache.contains(path)) {
      val cachedDoc = cache(path)
      (cachedDoc.primary, cachedDoc)
    } else {
      val (proc, doc) = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          Some(path),
          name
      )
      cache += (path -> doc)
      (proc, doc)
    }
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document
    * @param name tool/workflow name, in case it is not specified in the document
    * @return a [[Process]]
    */
  def parseString(sourceCode: String, name: Option[String] = None): (Process, Document) = {
    parse(RootLoader.loadDocument(sourceCode, normalizedBaseUri, loadingOptions.orNull), None, name)
  }

  def parseImport(relPath: String): (Process, Document) = {
    val path = baseUri.map(uri => Paths.get(uri.resolve(relPath))).getOrElse(Paths.get(relPath))
    if (cache.contains(path)) {
      val cachedDoc = cache(path)
      (cachedDoc.primary, cachedDoc)
    } else {
      val (proc, doc) = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          Some(path),
          None
      )
      cache += (path -> doc)
      (proc, doc)
    }
  }
}
