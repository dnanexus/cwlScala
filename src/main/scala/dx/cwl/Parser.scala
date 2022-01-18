package dx.cwl

import dx.cwl.Document.{Document, DocumentAdder}
import dx.cwl.Utils.yamlToJson
import dx.util.{FileUtils, JsUtils}
import dx.yaml._

import java.net.URI
import java.nio.file.{Path, Paths}
import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, ExpressionToolImpl, OperationImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}
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

case class ParserResult(mainProcess: Option[Process],
                        document: Document,
                        namespaces: Option[JsValue] = None,
                        schemas: Option[JsValue] = None)

case class Parser(baseUri: Option[URI] = None,
                  loadingOptions: Option[LoadingOptions] = None,
                  schemaDefs: Map[String, CwlSchema] = Map.empty,
                  hintSchemas: Map[String, HintSchema] = Map.empty) {
  private var cache: Map[Path, ParserResult] = Map.empty
  private lazy val normalizedBaseUri = baseUri.map(Utils.normalizeUri).orNull

  def versionAndClassFromYaml(value: YamlValue,
                              mainId: Identifier = Identifier.MainId): (String, Option[String]) = {
    def yamlToString(yamlValue: YamlValue): String = {
      yamlValue match {
        case YamlString(s) => s
        case other         => throw new Exception(s"expected string not ${other}")
      }
    }
    val versionKey = YamlString("cwlVersion")
    val classKey = YamlString("class")
    val graphKey = YamlString("$graph")
    value match {
      case YamlObject(fields) if fields.contains(versionKey) =>
        val version = yamlToString(fields(versionKey))
        if (fields.contains(classKey)) {
          (version, Some(yamlToString(fields(classKey))))
        } else if (fields.contains(graphKey)) {
          val YamlArray(graph) = fields(graphKey)
          if (graph.size == 1) {
            val YamlObject(procFields) = graph.head
            (version, procFields.get(classKey).map(cls => yamlToString(cls)))
          } else {
            val cls = graph.collectFirst {
              case YamlObject(fields)
                  if fields.contains(classKey) && fields
                    .get(YamlString("id"))
                    .exists(id => Identifier.parse(yamlToString(id)).equals(mainId)) =>
                yamlToString(fields(classKey))
            }
            (version, cls)
          }
        } else {
          (version, None)
        }
      case _ => throw new Exception("invalid CWL document: missing 'cwlVersion'")
    }
  }

  def versionAndClassFromJson(value: JsValue,
                              mainId: Identifier = Identifier.MainId): (String, Option[String]) = {
    def jsvToString(jsv: JsValue): String = {
      jsv match {
        case JsString(s) => s
        case other       => throw new Exception(s"expected string not ${other}")
      }
    }
    value match {
      case JsObject(fields) if fields.contains("cwlVersion") =>
        val version = jsvToString(fields("cwlVersion"))
        if (fields.contains("class")) {
          (version, Some(jsvToString(fields("class"))))
        } else if (fields.contains("$graph")) {
          val JsArray(graph) = fields("$graph")
          if (graph.size == 1) {
            val JsObject(procFields) = graph.head
            (version, procFields.get("class").map(cls => jsvToString(cls)))
          } else {
            val cls = graph.collectFirst {
              case JsObject(fields)
                  if fields.contains("class") && fields
                    .get("id")
                    .exists(id => Identifier.parse(jsvToString(id)).equals(mainId)) =>
                jsvToString(fields("class"))
            }
            (version, cls)
          }
        } else {
          (version, None)
        }
      case _ => throw new Exception("invalid CWL document: missing 'cwlVersion'")
    }
  }

  /**
    * Can a string be parsed as CWL?
    */
  def detectVersionAndClass(sourceCode: String,
                            format: Option[String] = None,
                            mainId: Identifier = Identifier.MainId): (String, Option[String]) = {
    format match {
      case Some("yaml") => versionAndClassFromYaml(sourceCode.parseYaml, mainId)
      case Some("json") => versionAndClassFromJson(sourceCode.parseJson, mainId)
      case Some(other)  => throw new Exception(s"unsupported format ${other}")
      case None =>
        try {
          versionAndClassFromJson(sourceCode.parseJson, mainId)
        } catch {
          case _: Throwable =>
            versionAndClassFromYaml(sourceCode.parseYaml, mainId)
        }
    }
  }

  /**
    * Can a file be parsed as CWL?
    */
  def detectVersionAndClassFromFile(
      path: Path,
      mainId: Identifier = Identifier.MainId
  ): (String, Option[String]) = {
    if (path.toString.endsWith(".cwl")) {
      versionAndClassFromYaml(FileUtils.readFileContent(path).parseYaml, mainId)
    } else {
      versionAndClassFromJson(JsUtils.jsFromFile(path), mainId)
    }
  }

  private[cwl] def parse(doc: Object,
                         source: Option[Path] = None,
                         mainId: Option[Identifier] = None,
                         defaultNamespace: Option[String] = None,
                         defaultMainFrag: Option[String] = None,
                         isGraph: Boolean = false,
                         dependencies: Document = Document.empty,
                         rawProcesses: Map[Identifier, Object] = Map.empty): ParserResult = {
    doc match {
      case commandLineTool: CommandLineToolImpl =>
        val proc =
          CommandLineTool.parse(commandLineTool, this, source, defaultNamespace, defaultMainFrag)
        ParserResult(Some(proc), dependencies.addProcess(proc))
      case expressionTool: ExpressionToolImpl =>
        val proc =
          ExpressionTool.parse(expressionTool, this, source, defaultNamespace, defaultMainFrag)
        ParserResult(Some(proc), dependencies.addProcess(proc))
      case operation: OperationImpl =>
        val proc =
          Operation.parse(operation, this, source, defaultNamespace, defaultMainFrag)
        ParserResult(Some(proc), dependencies.addProcess(proc))
      case workflow: WorkflowImpl =>
        val (wf, doc) = Workflow.parse(workflow,
                                       this,
                                       source,
                                       defaultNamespace,
                                       defaultMainFrag,
                                       isGraph,
                                       dependencies,
                                       rawProcesses)
        ParserResult(Some(wf), doc)
      case graph: java.util.List[_] =>
        // this is the result of parsing a workflow packed as a $graph - if there is a process with ID "#main" then
        // use that as the main process
        val rawProcesses = graph.asScala.toVector
          .map {
            case tool: CommandLineToolImpl          => tool.getId.get() -> tool
            case workflow: WorkflowImpl             => workflow.getId.get() -> workflow
            case expressionTool: ExpressionToolImpl => expressionTool.getId.get() -> expressionTool
            case operation: OperationImpl           => operation.getId.get() -> operation
            case other =>
              throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
          }
          .map {
            case (id, process) =>
              Identifier.parse(id) -> process
          }
          .toMap
        val graphMainId = mainId.getOrElse {
          if (rawProcesses.size == 1) {
            rawProcesses.head._1
          } else if (rawProcesses.keySet.contains(Identifier.MainId)) {
            Identifier.MainId
          } else {
            val wfs = rawProcesses.collect {
              case (id, _: WorkflowImpl) => id
            }
            if (wfs.size == 1) {
              wfs.head
            } else {
              throw new Exception("cannot determine main process in graph")
            }
          }
        }
        val (mainProcess, newDependencies) =
          rawProcesses.foldLeft(Option.empty[Process], dependencies) {
            case ((main, accu), (id, _)) if accu.contains(id) => (main, accu)
            case ((main, accu), (id, rawProc)) =>
              (id, main) match {
                case (id, Some(_)) if id == graphMainId =>
                  throw new Exception(s"more than one process has ID ${graphMainId}")
                case (id, None) if id == graphMainId =>
                  val ParserResult(proc, newAccu, _, _) = parse(
                      rawProc,
                      source = source,
                      mainId = Some(graphMainId),
                      defaultNamespace = defaultNamespace,
                      defaultMainFrag = defaultMainFrag,
                      isGraph = true,
                      dependencies = accu,
                      rawProcesses = rawProcesses
                  )
                  (proc, newAccu)
                case _ =>
                  val ParserResult(_, newAccu, _, _) = parse(
                      rawProc,
                      mainId = Some(graphMainId),
                      defaultNamespace = defaultNamespace,
                      isGraph = true,
                      dependencies = accu,
                      rawProcesses = rawProcesses
                  )
                  (main, newAccu)
              }
          }
        ParserResult(mainProcess, newDependencies)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
    }
  }

  private[cwl] def parseImport(relPath: String,
                               dependencies: Document = Document.empty): ParserResult = {
    val path = baseUri.map(uri => Paths.get(uri.resolve(relPath))).getOrElse(Paths.get(relPath))
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          source = Some(path),
          defaultNamespace = Some(normalizedBaseUri),
          dependencies = dependencies
      )
      cache = cache + (path -> result)
      result
    }
  }

  /**
    * Parses a CWL document from a file.
    * @param path path to the document
    * @param mainId the identifier of the main process.
    * @param defaultMainFrag main process name, in case it is not specified in the document.
    * @return a [[ParserResult]]
    */
  def parseFile(path: Path,
                mainId: Option[Identifier] = None,
                defaultMainFrag: Option[String] = None): ParserResult = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          source = Some(path),
          mainId = mainId,
          defaultNamespace = Some(normalizedBaseUri),
          defaultMainFrag = defaultMainFrag
      )
      // get the $namespaces and $schemas from a packed workflow
      val finalResult = if (path.toString.endsWith(".cwl")) {
        val yamlDoc = FileUtils.readFileContent(path).parseYaml.asYamlObject.fields
        result.copy(namespaces = yamlDoc.get(YamlString("$namespaces")).map(yamlToJson),
                    schemas = yamlDoc.get(YamlString("$schemas")).map(yamlToJson))
      } else {
        val jsDoc = JsUtils.jsFromFile(path).asJsObject.fields
        result.copy(namespaces = jsDoc.get("$namespaces"), schemas = jsDoc.get("$schemas"))
      }
      cache = cache + (path -> finalResult)
      finalResult
    }
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document.
    * @param mainId the identifier of the main process.
    * @param defaultMainFrag main process name, in case it is not specified in the document.
    * @return a [[Process]]
    */
  def parseString(sourceCode: String,
                  mainId: Option[Identifier] = None,
                  defaultMainFrag: Option[String] = None): ParserResult = {
    val result = parse(
        RootLoader.loadDocument(sourceCode, normalizedBaseUri, loadingOptions.orNull),
        defaultNamespace = Some(normalizedBaseUri),
        mainId = mainId,
        defaultMainFrag = defaultMainFrag
    )
    // get the $namespaces and $schemas - we can't guess the format from the file extension so
    // we try both json and yaml
    try {
      val jsDoc = sourceCode.parseJson.asJsObject.fields
      result.copy(namespaces = jsDoc.get("$namespaces"), schemas = jsDoc.get("$schemas"))
    } catch {
      case _: Throwable =>
        val yamlDoc = sourceCode.parseYaml.asYamlObject.fields
        result.copy(namespaces = yamlDoc.get(YamlString("$namespaces")).map(yamlToJson),
                    schemas = yamlDoc.get(YamlString("$schemas")).map(yamlToJson))
    }
  }
}
