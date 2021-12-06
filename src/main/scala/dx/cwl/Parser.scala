package dx.cwl

import dx.cwl.Document.{Document, DocumentAdder}
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

  def parse(doc: Object,
            source: Option[Path] = None,
            defaultNamespace: Option[String] = None,
            defaultFrag: Option[String] = None,
            dependencies: Document = Document.empty,
            rawProcesses: Map[Identifier, Object] = Map.empty,
            isGraph: Boolean = false,
            mainId: Identifier = Identifier.MainId): ParserResult = {
    doc match {
      case commandLineTool: CommandLineToolImpl =>
        val proc = CommandLineTool.parse(commandLineTool,
                                         this,
                                         source,
                                         defaultNamespace,
                                         defaultFrag,
                                         isGraph,
                                         mainId)
        ParserResult(Some(proc), dependencies.addProcess(proc))
      case expressionTool: ExpressionToolImpl =>
        val proc = ExpressionTool.parse(expressionTool,
                                        this,
                                        source,
                                        defaultNamespace,
                                        defaultFrag,
                                        isGraph,
                                        mainId)
        ParserResult(Some(proc), dependencies.addProcess(proc))
      case operation: OperationImpl =>
        val proc =
          Operation.parse(operation, this, source, defaultNamespace, defaultFrag, isGraph, mainId)
        ParserResult(Some(proc), dependencies.addProcess(proc))
      case workflow: WorkflowImpl =>
        val (wf, doc) = Workflow.parse(workflow,
                                       this,
                                       source,
                                       defaultNamespace,
                                       defaultFrag,
                                       dependencies,
                                       rawProcesses,
                                       isGraph,
                                       mainId)
        ParserResult(Some(wf), doc)
      case graph: java.util.List[_] =>
        // this is the result of parsing a workflow packed as a $graph - if there is a process with ID "#main" then
        // use that as the main process
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
        val (mainProcess, newDependencies) =
          rawProcesses.foldLeft(Option.empty[Process], dependencies) {
            case ((main, accu), (id, _)) if accu.contains(id) => (main, accu)
            case ((main, accu), (id, rawProc)) =>
              (id, main) match {
                case (id, Some(_)) if id == mainId =>
                  throw new Exception(s"more than one process has ID ${mainId}")
                case (id, None) if id == mainId =>
                  val ParserResult(proc, newAccu, _, _) = parse(
                      rawProc,
                      source = source,
                      defaultNamespace = defaultNamespace,
                      defaultFrag = defaultFrag,
                      dependencies = accu,
                      rawProcesses = rawProcesses,
                      isGraph = true,
                      mainId = mainId
                  )
                  (proc, newAccu)
                case _ =>
                  val ParserResult(_, newAccu, _, _) = parse(
                      rawProc,
                      defaultNamespace = defaultNamespace,
                      dependencies = accu,
                      rawProcesses = rawProcesses,
                      isGraph = true,
                      mainId = mainId
                  )
                  (main, newAccu)
              }
          }
        ParserResult(mainProcess, newDependencies)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other.getClass}")
    }
  }

  /**
    * Parses a CWL document from a file.
    * @param path path to the document
    * @param defaultFrag tool/workflow name, in case it is not specified in the document.
    *                    If not specified, the name of the file without .cwl is used.
    * @param isPacked whether the input is in packed form; ignored if the input is a JSON
    *                 file with a top-level "\$graph" element.
    * @return a [[ParserResult]]
    */
  def parseFile(path: Path,
                defaultFrag: Option[String] = None,
                isPacked: Boolean = false,
                mainId: Identifier = Identifier.MainId): ParserResult = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val result = parse(
          RootLoader.loadDocument(path, normalizedBaseUri, loadingOptions.orNull),
          source = Some(path),
          defaultNamespace = Some(normalizedBaseUri),
          defaultFrag = defaultFrag,
          isGraph = isPacked,
          mainId = mainId
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
                  isPacked: Boolean = false,
                  mainId: Identifier = Identifier.MainId): ParserResult = {
    val result = parse(
        RootLoader.loadDocument(sourceCode, normalizedBaseUri, loadingOptions.orNull),
        defaultNamespace = Some(normalizedBaseUri),
        defaultFrag = defaultFrag,
        isGraph = isPacked,
        mainId = mainId
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
          source = Some(path),
          defaultNamespace = Some(normalizedBaseUri),
          dependencies = dependencies
      )
      cache = cache + (path -> result)
      result
    }
  }
}
