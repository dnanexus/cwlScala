package dx.cwl

import dx.cwl.Utils.{translateOptional, translateOptionalArray, translateOptionalObject}
import org.w3id.cwl.cwl1_2.{CWLVersion, LoadListingEnum, SecondaryFileSchemaImpl}

import java.net.URI
import java.nio.file.Path
import scala.util.matching.Regex

/**
  * An identifier of the form [namespace]#frag, where frag is a '/'-delimited string. For frag "foo/bar/baz",
  * parent="foo/bar" and name="baz".
  * Note that two `Identifier`s are considered equal if their `frag`s are equal - namespaces are not considered.
  */
case class Identifier(namespace: Option[String], frag: String) {
  lazy val fullyQualifiedName: String = s"${namespace.getOrElse("")}#${frag}"

  lazy val parent: Option[String] = {
    if (frag.contains('/')) {
      Some(frag.substring(0, frag.lastIndexOf('/')))
    } else {
      None
    }
  }

  lazy val parentId: Option[Identifier] = {
    parent.map(p => Identifier(namespace, p))
  }

  def hasParent: Boolean = parent.isDefined

  lazy val name: String = {
    if (frag.contains('/')) {
      frag.substring(frag.lastIndexOf('/') + 1)
    } else {
      frag
    }
  }

  def finalizeFrag: Identifier = {
    copy(frag = frag.replace("/", "_"))
  }

  override def hashCode(): Int = frag.hashCode

  override def equals(obj: Any): Boolean = {
    (this, obj) match {
      case (Identifier(_, frag1), Identifier(_, frag2)) => frag1 == frag2
      case _                                            => false
    }
  }

  def equalsWithNamespace(that: Identifier): Boolean = {
    (this, that) match {
      case (Identifier(Some(ns1), frag1), Identifier(Some(ns2), frag2)) =>
        ns1 == ns2 && frag1 == frag2
      case _ => this.frag == that.frag
    }
  }

  /**
    * Copy this identifier and simplify the namespace and/or frag.
    * @param dropNamespace drop the namespace
    * @param replacePrefix replace the prefix - tuple of (toRemove, toAdd), where toRemove is one of Left(false) -
    *                      don't remove any of the prefix, Left(true) - remove the entire prefix, or Right(string) where
    *                      string is the prefix to remove; and toAdd is an optional prefix to add after the removal.
    * @param simplifyAutoName if true, simplify any auto-generated IDs. Packing a workflow with `cwlpack --add-ids` will
    *                         add any missing process IDs with format {workflow_name}@step_{step_name}@{process_name},
    *                         where process_name is either the file name from which the process was imported, or the
    *                         generic "run". If the later, then the ID will be updated to {step_name}_run, otherwise
    *                         {process_name}.
    * @param dropCwlExtension if true, and if the ID ends in one of the standard CWL file extensions (.cwl, .cwl.json,
    *                         or .json), drop the extension.
    */
  def simplify(dropNamespace: Boolean = false,
               replacePrefix: (Either[Boolean, String], Option[String]) = (Left(false), None),
               simplifyAutoName: Boolean = false,
               dropCwlExtension: Boolean = false): Identifier = {
    Identifier(
        Option.when(!dropNamespace)(namespace).flatten,
        Identifier.simplifyFrag(frag, replacePrefix, simplifyAutoName, dropCwlExtension)
    )
  }
}

object Identifier {
  val CwlExtensions = Vector(".cwl", ".cwl.json", ".json")
  val MainFrag = "main"
  val MainId: Identifier = Identifier(namespace = None, frag = MainFrag)
  val importNamespaceRegex: Regex = "^(.+\\.(?:cwl|yml|yaml))/(.+)".r
  val splitFragRegex: Regex = "(.+/)?(.+)".r

  def stripCwlExtension(fileName: String): String = {
    CwlExtensions
      .collectFirst {
        case ext if fileName.endsWith(ext) => fileName.dropRight(ext.length)
      }
      .getOrElse(fileName)
  }

  def simplifyFrag(frag: String,
                   replacePrefix: (Either[Boolean, String], Option[String]) = (Left(false), None),
                   simplifyAutoName: Boolean = false,
                   dropCwlExtension: Boolean = false): String = {
    val (prefix, name) = (frag, replacePrefix) match {
      case (splitFragRegex(null, name), (Left(_) | Right(""), Some(toAdd))) => (Some(toAdd), name)
      case (splitFragRegex(null, name), _)                                  => (None, name)
      case (splitFragRegex(prefix, name), (Right(toDrop), Some(toAdd)))
          if prefix.startsWith(toDrop) =>
        (Some(s"${toAdd}${prefix.drop(toDrop.length)}"), name)
      case (splitFragRegex(prefix, name), (Right(toDrop), None)) if prefix.startsWith(toDrop) =>
        (Some(prefix.drop(toDrop.length)), name)
      case (splitFragRegex(prefix, name), (Left(false), Some(toAdd))) =>
        (Some(s"${toAdd}${prefix}"), name)
      case (splitFragRegex(_, name), (Left(true), toAdd)) => (toAdd, name)
      case (splitFragRegex(prefix, name), _)              => (Option(prefix), name)
      case _                                              => throw new Exception(s"invalid frag ${frag}")
    }
    val simpleName = if (simplifyAutoName) {
      val parts = name.split('@').toVector
      if (parts.size >= 3) {
        parts.indexOf("run", 2) match {
          case -1 => parts.last
          case i =>
            parts.drop(i - 1).map(s => if (s.startsWith("step_")) s.drop(5) else s).mkString("_")
        }
      } else {
        name
      }
    } else {
      name
    }
    val nameWithoutExt = if (dropCwlExtension) {
      stripCwlExtension(simpleName)
    } else {
      simpleName
    }
    s"${prefix.getOrElse("")}${nameWithoutExt}"
  }

  def fromUri(uri: URI): Identifier = {
    val (namespace, name) = Utils.normalizeAndSplitUri(uri)
    val frag = name.getOrElse(throw new Exception(s"uri ${uri} does not contain a fragment"))
    Identifier(namespace, frag)
  }

  def fromSource(source: Path, namespace: Option[String]): Identifier = {
    Identifier(namespace, stripCwlExtension(source.getFileName.toString))
  }

  def parseUri(uri: String,
               defaultNamespace: Option[String] = None): (Option[String], Option[String]) = {
    val (namespace, frag) =
      try {
        Utils.normalizeAndSplitUri(URI.create(uri))
      } catch {
        case _: IllegalArgumentException if uri.startsWith("_:") =>
          // this is a random id generated by the Java parser for an anonymous/inline process
          (None, Some(uri.drop(2)))
        case _: Throwable => Utils.splitUri(uri)
      }
    (namespace, frag)
  }

  def parse(uri: String, defaultNamespace: Option[String] = None): Identifier = {
    val (namespace, frag) = parseUri(uri, defaultNamespace)
    Identifier(namespace, frag.getOrElse(s"uri ${uri} does not contain a fragment"))
  }

  def get(id: java.util.Optional[String],
          defaultNamespace: Option[String],
          defaultFrag: Option[String] = None,
          source: Option[Path] = None): Option[Identifier] = {
    translateOptional(id).map {
      Identifier.parseUri(_, defaultNamespace) match {
        case (namespace, Some(frag))                 => Identifier(namespace, frag)
        case (namespace, _) if defaultFrag.isDefined => Identifier(namespace, defaultFrag.get)
        case (namespace, _) if source.isDefined      => fromSource(source.get, namespace)
        case _ =>
          throw new Exception(s"could not determine frag for id ${id}")
      }
    }
  }
}

object Document {
  type Document = Map[Identifier, Process]
  val empty: Document = Map.empty[Identifier, Process]

  implicit class DocumentAdder(doc: Document) {
    def addProcess(proc: Process): Document = {
      val id = proc.id
        .getOrElse(
            throw new Exception(s"process ${proc} has no ID")
        )
      if (!doc.contains(id)) {
        doc + (id -> proc)
      } else if (doc(id) == proc) {
        // two identical processes with the same id - most likely two separate imports of the same process
        doc
      } else {
        throw new Exception(
            s"two different processes have the same ID ${id}: ${doc(id)} != ${proc}"
        )
      }
    }
  }
}

trait Identifiable {
  val id: Option[Identifier]

  def getName: Option[String] = id.map(_.frag)

  def hasName: Boolean = getName.isDefined

  def frag: String = id.map(_.frag).getOrElse(throw new Exception(s"${this} has no ID"))

  def parent: Option[String] = {
    if (hasName) {
      id.flatMap(_.parent)
    } else {
      throw new Exception(s"${this} has no name")
    }
  }

  def name: String = {
    id.map(_.name).getOrElse(throw new Exception(s"${this} has no name"))
  }

  def copySimplifyIds(
      dropNamespace: Boolean,
      replacePrefix: (Either[Boolean, String], Option[String]),
      simplifyAutoNames: Boolean,
      dropCwlExtension: Boolean
  ): Identifiable
}

trait Meta extends Identifiable {
  val label: Option[String]
  val doc: Option[String]
}

trait Loadable {
  val loadContents: Boolean
  val loadListing: LoadListing.LoadListing
}

trait Parameter extends Meta {
  val cwlType: CwlType
  val secondaryFiles: Vector[SecondaryFile]
  val streamable: Boolean
}

trait InputParameter extends Parameter with Loadable {
  val default: Option[CwlValue]
  val format: Vector[CwlValue]
}

trait OutputParameter extends Parameter {
  val format: Option[CwlValue]
}

/**
  * Marker trait for top-level elements (CommandLineTool, Workflow, ExpressionTool, etc)
  */
trait Process extends Meta {
  val source: Option[String]
  val cwlVersion: Option[CWLVersion]
  val label: Option[String]
  val doc: Option[String]
  val intent: Vector[String]
  val inputs: Vector[InputParameter]
  val outputs: Vector[OutputParameter]
  val requirements: Vector[Requirement]
  val hints: Vector[Hint]

  // packing a workflow with `cwlpack --add-ids` automatically adds any missing IDs of the form
  // `(<workflow_filename>@step_<step_id>@)?<tool_filename>.cwl`. This function returns <tool_filename>.
  // If tool_filename is "run" (the default), then the step_id is prepended (e.g. "step1_run").
  def simpleName: String = {
    Identifier.simplifyFrag(name, simplifyAutoName = true, dropCwlExtension = true)
  }

  override def copySimplifyIds(dropNamespace: Boolean,
                               replacePrefix: (Either[Boolean, String], Option[String]),
                               simplifyAutoNames: Boolean,
                               dropCwlExtension: Boolean): Process

  protected def getIdAndChildReplacePrefix(
      dropNamespace: Boolean,
      replacePrefix: (Either[Boolean, String], Option[String]),
      simplifyAutoNames: Boolean,
      dropCwlExtension: Boolean
  ): (Option[Identifier], (Either[Boolean, String], Option[String])) = {
    val (prefixToDrop, prefixToAdd) = (id.map(_.frag), replacePrefix) match {
      case (_, (Left(false) | Right(""), toAdd)) => (None, toAdd)
      case (Some(Identifier.splitFragRegex(prefix, _)), (Left(true), toAdd)) =>
        (Option(prefix), toAdd)
      case (Some(Identifier.splitFragRegex(prefix, _)), (Right(toDrop), toAdd))
          if Option(prefix).exists(_.startsWith(toDrop)) =>
        (Some(toDrop), toAdd)
      case _ => (None, None)
    }
    val simplifiedId = id
      .map(
          _.simplify(dropNamespace,
                     (prefixToDrop.toRight(false), prefixToAdd),
                     simplifyAutoNames,
                     dropCwlExtension)
      )
      .map(_.finalizeFrag)

    (simplifiedId,
     (id.map(i => s"${i.frag}/").toRight(false), simplifiedId.map(i => s"${i.frag}/")))
  }
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#SecondaryFileSchema
case class SecondaryFile(pattern: CwlValue, required: CwlValue)

object SecondaryFile {
  def apply(secondaryFile: SecondaryFileSchemaImpl,
            schemaDefs: Map[String, CwlSchema],
            isInput: Boolean): SecondaryFile = {
    val required = translateOptionalObject(secondaryFile.getRequired)
      .map(CwlValue(_, schemaDefs))
      .getOrElse(BooleanValue(isInput))
    SecondaryFile(CwlValue(secondaryFile.getPattern, schemaDefs), required)
  }

  def applyArray(secondaryFiles: java.lang.Object,
                 schemaDefs: Map[String, CwlSchema],
                 isInput: Boolean): Vector[SecondaryFile] = {
    translateOptionalArray(secondaryFiles).map {
      case sf: SecondaryFileSchemaImpl => SecondaryFile(sf, schemaDefs, isInput)
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
