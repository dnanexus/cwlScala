package dx.cwl

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}
import java.nio.file.Path

import org.w3id.cwl.cwl1_2.{CommandLineToolImpl, WorkflowImpl}
import org.w3id.cwl.cwl1_2.utils.{LoadingOptions, RootLoader}
import org.yaml.snakeyaml.Yaml

/**
  * Marker trait for top-level elements (CommandLineTool, Workflow, ExpressionTool, etc)
  */
trait Process {
  val source: Option[String]
}

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

  def parse(doc: java.lang.Object): Process = {
    doc match {
      case tool: CommandLineToolImpl => CommandLineTool(tool)
      case workflow: WorkflowImpl    => Workflow(workflow)
      case other =>
        throw new RuntimeException(s"unexpected top-level element ${other}")
    }
  }

  /**
    * Parses a CWL document from a file.
    * @note currently, only CommandLineTool documents are supported.
    * @param path path to the document
    * @param baseUri base URI to use when importing documents
    * @param loadingOptions document loading options
    * @return a [[Process]]
    */
  def parseFile(path: Path,
                baseUri: Option[String] = None,
                loadingOptions: Option[LoadingOptions] = None): Process = {
    parse(RootLoader.loadDocument(path, baseUri.orNull, loadingOptions.orNull))
  }

  /**
    * Parses a CWL document from a string.
    * @note currently, only CommandLineTool documents are supported.
    * @param sourceCode path to the document
    * @param baseUri base URI to use when importing documents
    * @param loadingOptions document loading options
    * @return a [[Process]]
    */
  def parseString(sourceCode: String,
                  baseUri: Option[String] = None,
                  loadingOptions: Option[LoadingOptions] = None): Process = {
    parse(RootLoader.loadDocument(sourceCode, baseUri.orNull, loadingOptions.orNull))
  }
}
