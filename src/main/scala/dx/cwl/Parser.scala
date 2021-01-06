package dx.cwl

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}
import java.nio.file.Path
import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, ExpressionToolImpl, OperationImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}
import org.yaml.snakeyaml.Yaml

object Parser {

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

  def parseDocument(doc: java.lang.Object,
                    source: Option[Path] = None,
                    schemaDefs: Map[String, CwlSchema] = Map.empty,
                    hintSchemas: Map[String, HintSchema] = Map.empty,
                    name: Option[String] = None): Process = {
    doc match {
      case tool: CommandLineToolImpl =>
        CommandLineTool(tool, source, schemaDefs, hintSchemas, name)
      case workflow: WorkflowImpl =>
        Workflow(workflow, source, schemaDefs, hintSchemas, name)
      case expressionTool: ExpressionToolImpl =>
        ExpressionTool(expressionTool, source, schemaDefs, hintSchemas, name)
      case operation: OperationImpl =>
        Operation(operation, source, schemaDefs, hintSchemas, name)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other}")
    }
  }

  def parse(doc: java.lang.Object,
            source: Option[Path] = None,
            schemaDefs: Vector[CwlSchema] = Vector.empty,
            hintSchemas: Vector[HintSchema] = Vector.empty,
            name: Option[String] = None): Process = {
    val schemaDefMap = schemaDefs.collect {
      case schema if schema.name.isDefined => schema.name.get -> schema
    }.toMap
    val hintSchemaMap = hintSchemas.map(s => s.className -> s).toMap
    parseDocument(doc, source, schemaDefMap, hintSchemaMap, name)
  }

  /**
    * Parses a CWL document from a file.
    * @note currently, only CommandLineTool documents are supported.
    * @param path path to the document
    * @param baseUri base URI to use when importing documents
    * @param loadingOptions document loading options
    * @param hintSchemas HintSchemas
    * @param name tool/workflow name, in case it is not specified in the document
    *             if not specified, the name of the file without .cwl is used
    * @return a [[Process]]
    */
  def parseFile(path: Path,
                baseUri: Option[String] = None,
                loadingOptions: Option[LoadingOptions] = None,
                schemaDefs: Vector[CwlSchema] = Vector.empty,
                hintSchemas: Vector[HintSchema] = Vector.empty,
                name: Option[String] = None): Process = {
    parse(RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull),
          Some(path),
          schemaDefs,
          hintSchemas,
          name)
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document
    * @param baseUri base URI to use when importing documents
    * @param loadingOptions document loading options
    * @param hintSchemas HintSchemas
    * @param name tool/workflow name, in case it is not specified in the document
    * @return a [[Process]]
    */
  def parseString(sourceCode: String,
                  baseUri: Option[String] = None,
                  loadingOptions: Option[LoadingOptions] = None,
                  schemaDefs: Vector[CwlSchema] = Vector.empty,
                  hintSchemas: Vector[HintSchema] = Vector.empty,
                  name: Option[String] = None): Process = {
    parse(RootLoader.loadDocument(sourceCode, baseUri.orNull, loadingOptions.orNull),
          None,
          schemaDefs,
          hintSchemas,
          name)
  }
}
