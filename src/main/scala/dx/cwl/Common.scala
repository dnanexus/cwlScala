package dx.cwl

import dx.cwl.Utils.{translateOptional, translateOptionalArray}
import org.w3id.cwl.cwl1_2.{CWLVersion, LoadListingEnum, SecondaryFileSchemaImpl}

import java.nio.file.Path

case class Identifier(namespace: Option[String], name: Option[String]) {
  def fullyQualifiedName: Option[String] =
    name.map(n => namespace.map(ns => s"${ns}#${n}").getOrElse(n))
}

object Identifier {
  private val identifierRegexp = "(.*?)(?:#(.*))?".r

  def apply(uri: String): Identifier = {
    uri match {
      case identifierRegexp(namespace, null) => Identifier(Some(namespace), None)
      case identifierRegexp(null, id)        => Identifier(None, Some(id))
      case identifierRegexp(namespace, id)   => Identifier(Some(namespace), Some(id))
      case _                                 => throw new Exception(s"invalid identifier ${uri}")
    }
  }

  def apply(id: java.util.Optional[String],
            name: Option[String] = None,
            source: Option[Path] = None): Identifier = {
    translateOptional(id).map(Identifier(_)) match {
      case Some(id) if id.name.isDefined => id
      case id if name.isDefined =>
        id.map(_.copy(name = name)).getOrElse(Identifier(namespace = None, name = name))
      case id if source.isDefined =>
        val name = Some(source.get.getFileName.toString.dropRight(4))
        id.map(_.copy(name = name)).getOrElse(Identifier(namespace = None, name = name))
      case _ =>
        throw new Exception("either tool id or file path must be defined")
    }
  }
}

trait Parameter {
  val id: Option[Identifier]
  val label: Option[String]
  val doc: Option[String]
  val types: Vector[CwlType]
  val secondaryFiles: Vector[SecondaryFile]
  val streamable: Option[Boolean]

  def getName: Option[String] = id.flatMap(_.name)
}

/**
  * Marker trait for top-level elements (CommandLineTool, Workflow, ExpressionTool, etc)
  */
trait Process {
  val source: Option[String]
  val cwlVersion: Option[CWLVersion]
  val id: Identifier
  val label: Option[String]
  val doc: Option[String]
  val intent: Vector[String]
  val inputs: Vector[Parameter]
  val outputs: Vector[Parameter]
  val requirements: Vector[Requirement]
  val hints: Vector[Hint]

  def name: String = id.name.getOrElse(throw new Exception("process has no name"))
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#SecondaryFileSchema
case class SecondaryFile(pattern: CwlValue, required: CwlValue)

object SecondaryFile {
  def apply(secondaryFile: SecondaryFileSchemaImpl,
            schemaDefs: Map[String, CwlSchema]): SecondaryFile = {
    SecondaryFile(CwlValue(secondaryFile.getPattern, schemaDefs),
                  CwlValue(secondaryFile.getRequired, schemaDefs))
  }

  def applyArray(secondaryFiles: java.lang.Object,
                 schemaDefs: Map[String, CwlSchema]): Vector[SecondaryFile] = {
    translateOptionalArray(secondaryFiles).map {
      case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected SecondaryFile value ${other}")
    }
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#LoadListingEnum
object LoadListing extends Enumeration {
  type LoadListing = Value
  val No, Shallow, Deep = Value

  def from(loadListing: LoadListingEnum): LoadListing = {
    loadListing match {
      case LoadListingEnum.NO_LISTING      => No
      case LoadListingEnum.SHALLOW_LISTING => Shallow
      case LoadListingEnum.DEEP_LISTING    => Deep
    }
  }
}
