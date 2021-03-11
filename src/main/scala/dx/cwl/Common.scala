package dx.cwl

import dx.cwl.Utils.{translateOptional, translateOptionalArray}
import org.w3id.cwl.cwl1_2.{CWLVersion, LoadListingEnum, SecondaryFileSchemaImpl}

import java.nio.file.Path

case class Identifier(namespace: Option[String], name: Option[String]) {
  def fullyQualifiedName: Option[String] = {
    name.map(n => namespace.map(ns => s"${ns}#${n}").getOrElse(n))
  }

  def unqualifiedName: Option[String] = {
    name.map {
      case n if n.contains('/') => n.substring(n.lastIndexOf('/') + 1)
      case n                    => n
    }
  }
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
    get(id, name, source) match {
      case _ =>
        throw new Exception("either tool id or file path must be defined")
    }
  }

  def get(id: java.util.Optional[String],
          name: Option[String] = None,
          source: Option[Path] = None): Option[Identifier] = {
    translateOptional(id).map(Identifier(_)) match {
      case Some(id) if id.name.isDefined => Some(id)
      case id if name.isDefined =>
        id.map(_.copy(name = name)).orElse(Some(Identifier(namespace = None, name = name)))
      case id if source.isDefined =>
        val name = Some(source.get.getFileName.toString.dropRight(4))
        id.map(_.copy(name = name)).orElse(Some(Identifier(namespace = None, name = name)))
      case _ => None
    }
  }
}

trait Identifiable {
  val id: Option[Identifier]

  def getName: Option[String] = id.flatMap(_.name)

  def hasName: Boolean = getName.isDefined

  def name: String = id.flatMap(_.name).getOrElse(throw new Exception(s"${this} has no name"))
}

trait Parameter extends Identifiable {
  val label: Option[String]
  val doc: Option[String]
  val cwlType: CwlType
  val secondaryFiles: Vector[SecondaryFile]
  val streamable: Option[Boolean]
}

trait InputParameter extends Parameter {
  val default: Option[CwlValue]
  val format: Vector[CwlValue]
  val loadContents: Option[Boolean]
  val loadListing: Option[LoadListing.LoadListing]
}

/**
  * Marker trait for top-level elements (CommandLineTool, Workflow, ExpressionTool, etc)
  */
trait Process extends Identifiable {
  val source: Option[String]
  val cwlVersion: Option[CWLVersion]
  val label: Option[String]
  val doc: Option[String]
  val intent: Vector[String]
  val inputs: Vector[Parameter]
  val outputs: Vector[Parameter]
  val requirements: Vector[Requirement]
  val hints: Vector[Hint]
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
