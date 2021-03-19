package dx.cwl

import dx.cwl.Utils.{translateOptional, translateOptionalArray}
import org.w3id.cwl.cwl1_2.{CWLVersion, LoadListingEnum, SecondaryFileSchemaImpl}

import java.net.URI
import java.nio.file.Path

/**
  * An identifier of the form [\[namespace]#][frag], where frag is a
  * '/'-delimited string. For frag "foo/bar/baz", parent="foo/bar"
  * and name="baz".
  */
case class Identifier(namespace: Option[String], frag: Option[String]) {
  def fullyQualifiedName: Option[String] = {
    frag.map(n => namespace.map(ns => s"${ns}#${n}").getOrElse(n))
  }

  def parent: Option[String] = {
    frag.flatMap {
      case n if n.contains('/') => Some(n.substring(0, n.lastIndexOf('/')))
      case _                    => None
    }
  }

  def name: Option[String] = {
    frag.map {
      case n if n.contains('/') => n.substring(n.lastIndexOf('/') + 1)
      case n                    => n
    }
  }
}

object Identifier {
  private val identifierRegexp = "(.*?)(?:#(.*))?".r

  def fromUri(uri: String): Identifier = {
    val (namespace, name) =
      try {
        Utils.normalizeAndSplitUri(URI.create(uri))
      } catch {
        case _: Throwable => (None, Some(uri))
      }
    Identifier(namespace, name)
  }

  def fromUri(uri: URI): Identifier = {
    val (namespace, name) = Utils.normalizeAndSplitUri(uri)
    Identifier(namespace, name)
  }

  def parse(uri: String, stripFragPrefix: Option[String] = None): Identifier = {
    val id = uri match {
      case identifierRegexp(namespace, null) => Identifier(Some(namespace), None)
      case identifierRegexp(null, id)        => Identifier(None, Some(id))
      case identifierRegexp(namespace, id)   => Identifier(Some(namespace), Some(id))
      case _                                 => throw new Exception(s"invalid identifier ${uri}")
    }
    stripFragPrefix
      .map { s =>
        assert(s.endsWith("/"))
        id.copy(frag = id.frag.map {
          case p if p.startsWith(s) => p.drop(s.length)
          case p =>
            throw new Exception(s"frag ${p} does not start with prefix ${s}")
        })
      }
      .getOrElse(id)
  }

  def get(id: java.util.Optional[String],
          frag: Option[String] = None,
          source: Option[Path] = None): Option[Identifier] = {
    translateOptional(id).map(Identifier.parse(_)) match {
      case Some(id) if id.frag.isDefined => Some(id)
      case id if frag.isDefined =>
        id.map(_.copy(frag = frag)).orElse(Some(Identifier(namespace = None, frag = frag)))
      case id if source.isDefined =>
        val name = Some(source.get.getFileName.toString.dropRight(4))
        id.map(_.copy(frag = name)).orElse(Some(Identifier(namespace = None, frag = name)))
      case _ => None
    }
  }
}

trait Identifiable {
  val id: Option[Identifier]

  def getName: Option[String] = id.flatMap(_.frag)

  def hasName: Boolean = getName.isDefined

  def frag: String = id.flatMap(_.frag).getOrElse(throw new Exception(s"${this} has no name"))

  def parent: Option[String] = {
    if (hasName) {
      id.flatMap(_.parent)
    } else {
      throw new Exception(s"${this} has no name")
    }
  }

  def name: String =
    id.flatMap(_.name).getOrElse(throw new Exception(s"${this} has no name"))
}

case class Document(processes: Map[String, Process], primaryId: String = "main") {
  def add(proc: Process, isPrimary: Boolean = false): Document = {
    val id = proc.id.flatMap(_.frag).getOrElse(primaryId)
    if (processes.contains(id)) {
      throw new Exception(s"two processes have the same ID ${id}")
    }
    val newPrimaryId = if (isPrimary) id else primaryId
    Document(processes + (id -> proc), newPrimaryId)
  }

  def isEmpty: Boolean = processes.isEmpty

  def contains(frag: String): Boolean = {
    processes.contains(frag)
  }

  def get(frag: String): Option[Process] = {
    processes.get(frag)
  }

  def apply(frag: String): Process = {
    processes(frag)
  }

  def primary: Process = {
    if (processes.isEmpty) {
      throw new Exception("no processes")
    } else if (processes.size == 1) {
      processes.values.head
    } else {
      processes.getOrElse(primaryId, throw new Exception(s"no process with ID '${primaryId}'"))
    }
  }
}

object Document {
  def empty: Document = Document(Map.empty)
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

trait OutputParameter extends Parameter {
  val format: Option[CwlValue]
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
  val inputs: Vector[InputParameter]
  val outputs: Vector[OutputParameter]
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
