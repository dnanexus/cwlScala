package dx.cwl

import dx.cwl.Document.{Document, DocumentAdder}
import dx.util.JsUtils

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}
import java.net.URI
import java.nio.file.{Path, Paths}
import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, ExpressionToolImpl, OperationImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import spray.json._

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

case class ParserResult(process: Process,
                        document: Document,
                        namespaces: Option[JsValue] = None,
                        schemas: Option[JsValue] = None)

case class Parser(baseUri: Option[URI] = None,
                  loadingOptions: Option[LoadingOptions] = None,
                  schemaDefs: Map[String, CwlSchema] = Map.empty,
                  hintSchemas: Map[String, HintSchema] = Map.empty) {
  private var cache: Map[Path, ParserResult] = Map.empty
  private lazy val normalizedBaseUri = baseUri.map(Utils.normalizeUri).orNull

  def versionAndClassFromYaml(inputStream: InputStream): Option[(String, String)] = {
    try {
      val yamlLoader = new Load(LoadSettings.builder().build())
      val doc = yamlLoader.loadFromInputStream(inputStream).asInstanceOf[java.util.Map[String, Any]]
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

  def versionAndClassFromJson(value: JsValue): Option[(String, String)] = {
    value match {
      case JsObject(fields) if fields.contains("cwlVersion") =>
        val JsString(version) = fields("cwlVersion")
        if (fields.contains("class")) {
          val JsString(cls) = fields("class")
          Some(version, cls)
        } else if (fields.contains("$graph")) {
          val JsArray(graph) = fields("$graph")
          graph.collectFirst {
            case JsObject(fields) if fields.get("id").contains(JsString(Identifier.MainFrag)) =>
              val JsString(cls) = fields("class")
              (version, cls)
          }
        } else {
          None
        }
      case _ => None
    }
  }

  /**
    * Can a file be parsed as CWL?
    */
  def detectVersionAndClass(path: Path): Option[(String, String)] = {
    if (path.toString.endsWith(".cwl")) {
      versionAndClassFromYaml(new FileInputStream(path.toFile))
    } else {
      versionAndClassFromJson(JsUtils.jsFromFile(path))
    }
  }

  /**
    * Can a string be parsed as CWL?
    */
  def detectVersionAndClass(sourceCode: String,
                            format: Option[String] = None): Option[(String, String)] = {
    format match {
      case Some("yaml") => versionAndClassFromYaml(new ByteArrayInputStream(sourceCode.getBytes()))
      case Some("json") => versionAndClassFromJson(sourceCode.parseJson)
      case Some(other)  => throw new Exception(s"unsupported format ${other}")
      case None =>
        try {
          versionAndClassFromJson(sourceCode.parseJson)
        } catch {
          case _: Throwable =>
            versionAndClassFromYaml(new ByteArrayInputStream(sourceCode.getBytes()))
        }
    }
  }

  def parse(doc: Object,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            dependencies: Document = Document.empty,
            rawProcesses: Map[Identifier, Object] = Map.empty,
            isGraph: Boolean = false): ParserResult = {
    doc match {
      case tool: CommandLineToolImpl =>
        val proc = CommandLineTool(tool, this, source, defaultNamespace, defaultFrag, isGraph)
        ParserResult(proc, dependencies.addProcess(proc))
      case expressionTool: ExpressionToolImpl =>
        val proc =
          ExpressionTool(expressionTool, this, source, defaultNamespace, defaultFrag, isGraph)
        ParserResult(proc, dependencies.addProcess(proc))
      case operation: OperationImpl =>
        val proc = Operation(operation, this, source, defaultNamespace, defaultFrag, isGraph)
        ParserResult(proc, dependencies.addProcess(proc))
      case workflow: WorkflowImpl =>
        val (wf, doc) = Workflow.parse(workflow,
                                       this,
                                       source,
                                       defaultNamespace,
                                       defaultFrag,
                                       dependencies,
                                       rawProcesses,
                                       isGraph)
        ParserResult(wf, doc)
      case graph: java.util.List[_] =>
        // this is the result of parsing a packed workflow - the
        // top-level process will be called "main".
        val rawProcesses = graph.asScala.toVector.map {
          case tool: CommandLineToolImpl =>
            Identifier.parse(tool.getId.get()) -> tool
          case workflow: WorkflowImpl =>
            Identifier.parse(workflow.getId.get()) -> workflow
          case expressionTool: ExpressionToolImpl =>
            Identifier.parse(expressionTool.getId.get()) -> expressionTool
          case operation: OperationImpl =>
            Identifier.parse(operation.getId.get()) -> operation
          case other =>
            throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
        }.toMap
        val (primaryProc, newDependencies) =
          rawProcesses.foldLeft(Option.empty[Process], dependencies) {
            case ((primaryProc, accu), (id, _)) if accu.contains(id) => (primaryProc, accu)
            case ((primaryProc, accu), (id, rawProc)) =>
              (id, primaryProc) match {
                case (id, Some(_)) if id.frag.contains(Identifier.Main) =>
                  throw new Exception("more than one process has ID frag='main'")
                case (id, None) if id.frag.contains(Identifier.Main) =>
                  val ParserResult(proc, newAccu, _, _) =
                    parse(rawProc,
                          source,
                          defaultNamespace,
                          defaultFrag,
                          accu,
                          rawProcesses,
                          isGraph = true)
                  (Some(proc), newAccu)
                case _ =>
                  val ParserResult(_, newAccu, _, _) =
                    parse(rawProc,
                          defaultNamespace = defaultNamespace,
                          dependencies = accu,
                          rawProcesses = rawProcesses,
                          isGraph = true)
                  (primaryProc, newAccu)
              }
          }
        ParserResult(
            primaryProc.getOrElse(throw new Exception("no process found with ID frag='main'")),
            newDependencies
        )
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
    }
  }

  /**
    * Parses a CWL document from a file.
    * @note currently, only CommandLineTool documents are supported.
    * @param path path to the document
    * @param defaultFrag tool/workflow name, in case it is not specified in the document.
    *                    If not specified, the name of the file without .cwl is used.
    * @param isPacked whether the input is in packed form; ignored if the input is a JSON
    *                 file with a top-level "\$graph" element.
    * @return a [[Process]]
    */
  def parseFile(path: Path,
                defaultFrag: Option[String] = None,
                isPacked: Boolean = false): ParserResult = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          Some(path),
          defaultNamespace = Some(normalizedBaseUri),
          defaultFrag,
          isGraph = isPacked
      )
      // get the $namespaces and $schemas from a packed workflow
      val finalResult = if (isPacked) {
        val jsDoc = JsUtils.jsFromFile(path).asJsObject.fields
        result.copy(namespaces = jsDoc.get("$namespaces"), schemas = jsDoc.get("$schemas"))
      } else {
        result
      }
      cache = cache + (path -> finalResult)
      finalResult
    }
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document
    * @param defaultFrag tool/workflow name, in case it is not specified in the document
    * @param isPacked whether the input is in packed form; ignored if the input is a JSON
    *                 file with a top-level "\$graph" element.
    * @return a [[Process]]
    */
  def parseString(sourceCode: String,
                  defaultFrag: Option[String] = None,
                  isPacked: Boolean = false): ParserResult = {
    val result = parse(
        RootLoader.loadDocument(sourceCode, normalizedBaseUri, loadingOptions.orNull),
        None,
        defaultNamespace = Some(normalizedBaseUri),
        defaultFrag,
        isGraph = isPacked
    )
    // get the $namespaces and $schemas from a packed workflow
    if (isPacked) {
      val jsDoc = sourceCode.parseJson.asJsObject.fields
      result.copy(namespaces = jsDoc.get("$namespaces"), schemas = jsDoc.get("$schemas"))
    } else {
      result
    }
  }

  def parseImport(relPath: String, dependencies: Document = Document.empty): ParserResult = {
    val path = baseUri.map(uri => Paths.get(uri.resolve(relPath))).getOrElse(Paths.get(relPath))
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          Some(path),
          defaultNamespace = Some(normalizedBaseUri),
          None,
          dependencies
      )
      cache = cache + (path -> result)
      result
    }
  }
}
