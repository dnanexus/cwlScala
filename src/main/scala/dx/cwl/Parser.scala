package dx.cwl

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}
import java.nio.file.Path
import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, ExpressionToolImpl, OperationImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}
import org.yaml.snakeyaml.Yaml

object Parser {
  lazy val default: Parser = Parser()

  def create(baseUri: Option[String] = None,
             loadingOptions: Option[LoadingOptions] = None,
             schemaDefs: Vector[CwlSchema] = Vector.empty,
             hintSchemas: Vector[HintSchema] = Vector.empty): Parser = {
    val schemaDefMap = schemaDefs.collect {
      case schema if schema.name.isDefined => schema.name.get -> schema
    }.toMap
    val hintSchemaMap = hintSchemas.map(s => s.className -> s).toMap
    Parser(baseUri, loadingOptions, schemaDefMap, hintSchemaMap)
  }
}

case class Parser(baseUri: Option[String] = None,
                  loadingOptions: Option[LoadingOptions] = None,
                  schemaDefs: Map[String, CwlSchema] = Map.empty,
                  hintSchemas: Map[String, HintSchema] = Map.empty) {
  private var cache: Map[Path, Process] = Map.empty

  def canParse(inputStream: InputStream): Boolean = {
    try {
      val doc = new Yaml().load[java.util.Map[String, Any]](inputStream)
      doc.containsKey("cwlVersion") && doc.get("cwlVersion").asInstanceOf[String].startsWith("v1.2")
    } catch {
      case _: Throwable =>
        false
    }
  }

  /**
    * Can a file be parsed as CWL?
    */
  def canParse(path: Path): Boolean = {
    canParse(new FileInputStream(path.toFile))
  }

  def canParse(sourceCode: String): Boolean = {
    canParse(new ByteArrayInputStream(sourceCode.getBytes()))
  }

  def parse(doc: java.lang.Object,
            source: Option[Path] = None,
            name: Option[String] = None): Process = {
    doc match {
      case tool: CommandLineToolImpl =>
        CommandLineTool(tool, this, source, name)
      case workflow: WorkflowImpl =>
        Workflow(workflow, this, source, name)
      case expressionTool: ExpressionToolImpl =>
        ExpressionTool(expressionTool, this, source, name)
      case operation: OperationImpl =>
        Operation(operation, this, source, name)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other}")
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
  def parseFile(path: Path, name: Option[String] = None): Process = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val doc = parse(RootLoader.loadDocument(path,
                                              baseUri.map(Utils.normalizeUri).orNull,
                                              loadingOptions.orNull),
                      Some(path),
                      name)
      cache += (path -> doc)
      doc
    }
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document
    * @param name tool/workflow name, in case it is not specified in the document
    * @return a [[Process]]
    */
  def parseString(sourceCode: String, name: Option[String] = None): Process = {
    parse(RootLoader.loadDocument(sourceCode,
                                  baseUri.map(Utils.normalizeUri).orNull,
                                  loadingOptions.orNull),
          None,
          name)
  }
}
