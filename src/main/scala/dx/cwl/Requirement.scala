package dx.cwl

import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{
  CommandInputSchema,
  DirentImpl,
  DockerRequirementImpl,
  EnvVarRequirementImpl,
  EnvironmentDefImpl,
  InitialWorkDirRequirementImpl,
  InlineJavascriptRequirementImpl,
  InplaceUpdateRequirementImpl,
  LoadListingRequirementImpl,
  NetworkAccessImpl,
  ProcessRequirement,
  ResourceRequirementImpl,
  SchemaDefRequirementImpl,
  ShellCommandRequirementImpl,
  SoftwarePackageImpl,
  SoftwareRequirementImpl,
  ToolTimeLimitImpl,
  WorkReuseImpl
}

import scala.jdk.CollectionConverters._

/**
  * A hint, which may be one of the Requirements defined in the spec or
  * base on a user-defined schema.
  */
trait Hint

trait HintSchema {
  val className: String = getClass.getName

  def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint
}

/**
  * One of the requirements defined in the CWL spec.
  * https://www.commonwl.org/v1.2/CommandLineTool.html#InlineJavascriptRequirement
  */
sealed trait Requirement extends Hint

case class GenericHint(attributes: Map[String, Any]) extends Hint

object Requirement {
  def apply(requirement: ProcessRequirement, schemaDefs: Map[String, CwlSchema]): Requirement = {
    requirement match {
      case req: InlineJavascriptRequirementImpl => InlineJavascriptRequirement(req)
      case req: SchemaDefRequirementImpl        => SchemaDefRequirement(req, Map.empty)
      case req: LoadListingRequirementImpl      => LoadListingRequirement(req)
      case req: DockerRequirementImpl           => DockerRequirement(req)
      case req: SoftwareRequirementImpl         => SoftwareRequirement(req)
      case req: InitialWorkDirRequirementImpl   => InitialWorkDirRequirement(req, schemaDefs)
      case req: EnvVarRequirementImpl           => EnvVarRequirement(req, schemaDefs)
      case _: ShellCommandRequirementImpl       => ShellCommandRequirement
      case req: ResourceRequirementImpl         => ResourceRequirement(req, schemaDefs)
      case req: WorkReuseImpl                   => WorkReuseRequirement(req, schemaDefs)
      case req: NetworkAccessImpl               => NetworkAccessRequirement(req, schemaDefs)
      case req: InplaceUpdateRequirementImpl    => InplaceUpdateRequirement(req)
      case req: ToolTimeLimitImpl               => ToolTimeLimitRequirement(req, schemaDefs)
      case _ =>
        throw new RuntimeException(s"unsupported requirement value ${requirement}")
    }
  }

  val DefaultHintSchemas: Map[String, HintSchema] = Vector(
      InlineJavascriptRequirement,
      LoadListingRequirement,
      DockerRequirement,
      SoftwareRequirement,
      InitialWorkDirRequirement,
      EnvVarRequirement,
      ShellCommandRequirement,
      ResourceRequirement,
      WorkReuseRequirement,
      NetworkAccessRequirement,
      InplaceUpdateRequirement,
      ToolTimeLimitRequirement
  ).map(schema => schema.className -> schema).toMap

  def apply(hint: Map[String, Any],
            schemaDefs: Map[String, CwlSchema],
            hintSchemas: Map[String, HintSchema]): Hint = {
    hint.get("class") match {
      case Some(cls: String) if hintSchemas.contains(cls) =>
        hintSchemas(cls).apply(hint, schemaDefs)
      case Some(cls: String) if DefaultHintSchemas.contains(cls) =>
        DefaultHintSchemas(cls).apply(hint, schemaDefs)
      case _ => GenericHint(hint)
    }
  }

  /**
    * Compares two literal numeric values to test if the first is
    * less than or equal to the second. Returns true of either
    * value is not a literal numeric value.
    * @param min lesser value to test
    * @param max greater value to test
    * @return
    */
  def lte(min: Option[CwlValue], max: Option[CwlValue]): Boolean = {
    (min, max) match {
      case (Some(min: NumericValue), Some(max: NumericValue)) =>
        min.decimalValue <= max.decimalValue
      case _ => true
    }
  }

  /**
    * Ensures that min and max are both >= defaultMinValue, that
    * min <= max, and that min is always set.
    * @param min min value
    * @param max max value
    * @param defaultMinValue default min value
    * @return
    */
  def updateMinMax(min: Option[CwlValue],
                   max: Option[CwlValue],
                   defaultMinValue: Long): (Option[CwlValue], Option[CwlValue]) = {
    val updatedMin = min.map {
      case m: NumericValue if m.decimalValue < defaultMinValue => LongValue(defaultMinValue)
      case m                                                   => m
    }
    val updatedMax = max.map {
      case m: NumericValue if m.decimalValue < defaultMinValue => LongValue(defaultMinValue)
      case m                                                   => m
    }
    (updatedMin, updatedMax) match {
      case (Some(min: NumericValue), Some(max: NumericValue))
          if min.decimalValue > max.decimalValue =>
        (Some(min), Some(min))
      case other => other
    }
  }
}

case class InlineJavascriptRequirement(expressionLib: Option[String]) extends Requirement

object InlineJavascriptRequirement extends HintSchema {
  def apply(req: InlineJavascriptRequirementImpl): InlineJavascriptRequirement = {
    val exprLib = translateOptionalArray(req.getExpressionLib).asOption
      .map(_.iterator.map(translateString).mkString("\n"))
    InlineJavascriptRequirement(exprLib)
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    InlineJavascriptRequirement(expressionLib = hint.get("expressionLib").map(_.toString))
  }
}

case class SchemaDefRequirement(typeDefinitions: Vector[CwlSchema]) extends Requirement {
  def asMap: Map[String, CwlSchema] = {
    typeDefinitions.map(schema => schema.name.get -> schema)
  }.toMap
}

object SchemaDefRequirement {
  def apply(req: SchemaDefRequirementImpl,
            schemaDefs: Map[String, CwlSchema]): SchemaDefRequirement = {
    val typeDefs: Vector[CwlSchema] = req.getTypes.asScala.map {
      case schema: CommandInputSchema =>
        val cwlSchema = CwlSchema(schema, schemaDefs)
        if (cwlSchema.name.isEmpty) {
          throw new Exception(s"schema name must be defined in SchemaDefRequirement ${req}")
        }
        cwlSchema
      case other =>
        throw new Exception(s"unexpected type definition ${other}")
    }.toVector
    SchemaDefRequirement(typeDefs)
  }
}

case class LoadListingRequirement(value: Option[LoadListing.LoadListing]) extends Requirement

object LoadListingRequirement extends HintSchema {
  def apply(req: LoadListingRequirementImpl): LoadListingRequirement = {
    LoadListingRequirement(translateOptional(req.getLoadListing).map(LoadListing.from))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    LoadListingRequirement(value =
      hint.get("loadListing").map(name => LoadListing.withName(name.toString))
    )
  }
}

case class DockerRequirement(pullName: Option[String],
                             loadUri: Option[String],
                             importUri: Option[String],
                             dockerfile: Option[String],
                             imageId: Option[String],
                             outputDirectory: Option[String])
    extends Requirement

object DockerRequirement extends HintSchema {
  def apply(req: DockerRequirementImpl): DockerRequirement = {
    DockerRequirement(
        translateOptional(req.getDockerPull).map(translateString),
        translateOptional(req.getDockerLoad).map(translateString),
        translateOptional(req.getDockerImport).map(translateString),
        translateOptional(req.getDockerFile).map(translateString),
        translateOptional(req.getDockerImageId).map(translateString),
        translateOptional(req.getDockerOutputDirectory).map(translateString)
    )
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    DockerRequirement(
        pullName = hint.get("dockerPull").map(_.toString),
        loadUri = hint.get("dockerLoad").map(_.toString),
        importUri = hint.get("dockerImport").map(_.toString),
        dockerfile = hint.get("dockerFile").map(_.toString),
        imageId = hint.get("dockerImageId").map(_.toString),
        outputDirectory = hint.get("dockerOutputDirectory").map(_.toString)
    )
  }
}

case class SoftwarePackage(name: String, version: Vector[String], specs: Vector[String])
    extends Requirement

object SoftwarePackage {
  def apply(pkg: SoftwarePackageImpl): SoftwarePackage = {
    SoftwarePackage(pkg.getPackage,
                    translateOptionalArray(pkg.getVersion).map(translateString),
                    translateOptionalArray(pkg.getSpecs).map(translateString))
  }

  def apply(hint: Map[String, Any]): SoftwarePackage = {
    SoftwarePackage(
        name =
          hint.getOrElse("name", throw new Exception("missing required attribute name")).toString,
        version = hint
          .get("version")
          .map {
            case versions: Seq[Any] => versions.map(_.toString).toVector
            case other              => throw new Exception(s"invalid version ${other}")
          }
          .getOrElse(Vector.empty),
        specs = hint
          .get("specs")
          .map {
            case versions: Seq[Any] => versions.map(_.toString).toVector
            case other              => throw new Exception(s"invalid version ${other}")
          }
          .getOrElse(Vector.empty)
    )
  }
}

case class SoftwareRequirement(packages: Vector[SoftwarePackage]) extends Requirement

object SoftwareRequirement extends HintSchema {
  def apply(req: SoftwareRequirementImpl): SoftwareRequirement = {
    SoftwareRequirement(
        req.getPackages.asScala.map {
          case pkg: SoftwarePackageImpl => SoftwarePackage(pkg)
          case other =>
            throw new RuntimeException(s"unexpected SoftwarePackage value ${other}")
        }.toVector
    )
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    SoftwareRequirement(
        packages = hint
          .getOrElse("packages", throw new Exception("missing required attribute packages")) match {
          case packages: Seq[Any] =>
            packages.map {
              case pkg: Map[_, _] => SoftwarePackage(toStringAnyMap(pkg))
            }.toVector
          case other =>
            throw new RuntimeException(s"unexpected package value ${other}")
        }
    )
  }
}

sealed trait InitialWorkDirEntry
case class ValueInitialWorkDirEntry(value: CwlValue) extends InitialWorkDirEntry
case class DirInitialWorkDirEntry(entry: CwlValue,
                                  entryName: Option[CwlValue],
                                  writable: Option[Boolean])
    extends InitialWorkDirEntry

object DirInitialWorkDirEntry {
  def apply(dirent: DirentImpl, schemaDefs: Map[String, CwlSchema]): DirInitialWorkDirEntry = {
    val entry = CwlValue(dirent.getEntry, schemaDefs)
    val entryName = translateOptionalObject(dirent.getEntryname).map(CwlValue(_, schemaDefs))
    val writable = translateOptional[java.lang.Boolean](dirent.getWritable).map(_.booleanValue())
    DirInitialWorkDirEntry(entry, entryName, writable)
  }

  def apply(dirent: Map[String, Any],
            schemaDefs: Map[String, CwlSchema]): DirInitialWorkDirEntry = {
    DirInitialWorkDirEntry(
        entry = CwlValue(
            dirent
              .getOrElse("entry", throw new Exception("missing required attribute 'entry'")),
            schemaDefs
        ),
        entryName = dirent.get("entryname").map(CwlValue(_, schemaDefs)),
        writable = dirent.get("writable").map {
          case s: String  => s.toBoolean
          case b: Boolean => b
          case other      => throw new Exception(s"invalid writable value ${other}")
        }
    )
  }
}

case class InitialWorkDirRequirement(listing: Vector[InitialWorkDirEntry]) extends Requirement

object InitialWorkDirRequirement extends HintSchema {
  def apply(req: InitialWorkDirRequirementImpl,
            schemaDefs: Map[String, CwlSchema]): InitialWorkDirRequirement = {
    InitialWorkDirRequirement(translateArray(req.getListing).map {
      case entry: DirentImpl => DirInitialWorkDirEntry(entry, schemaDefs)
      case value             => ValueInitialWorkDirEntry(CwlValue(value, schemaDefs))
    })
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    InitialWorkDirRequirement(
        listing = hint
          .getOrElse("listing", throw new Exception("missing required attribute 'listing'")) match {
          case entry: Seq[_] =>
            entry.map {
              case m: Map[_, _] => DirInitialWorkDirEntry.apply(toStringAnyMap(m), schemaDefs)
              case other        => throw new Exception(s"invalid entry value ${other}")
            }.toVector
          case value: Object =>
            Vector(ValueInitialWorkDirEntry(CwlValue(value, schemaDefs)))
          case other =>
            throw new RuntimeException(s"unexpected listing value ${other}")
        }
    )
  }
}

case class EnvironmentDefinition(name: String, value: CwlValue)

object EnvironmentDefinition {
  def apply(env: EnvironmentDefImpl, schemaDefs: Map[String, CwlSchema]): EnvironmentDefinition = {
    EnvironmentDefinition(env.getEnvName, CwlValue(env.getEnvValue, schemaDefs))
  }
}

case class EnvVarRequirement(environmentDefinitions: Vector[EnvironmentDefinition])
    extends Requirement

object EnvVarRequirement extends HintSchema {
  def apply(req: EnvVarRequirementImpl, schemaDefs: Map[String, CwlSchema]): EnvVarRequirement = {
    EnvVarRequirement(req.getEnvDef.asScala.map {
      case env: EnvironmentDefImpl => EnvironmentDefinition(env, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected EnvironmentDef value ${other}")
    }.toVector)
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    EnvVarRequirement(
        hint.getOrElse("envDef", throw new Exception("missing required attribute 'envDef'")) match {
          case defs: Seq[_] =>
            defs.map {
              case d: Map[_, _] =>
                val dStrAny = toStringAnyMap(d)
                EnvironmentDefinition(dStrAny("envName").toString,
                                      CwlValue(dStrAny("envValue"), schemaDefs))
              case other =>
                throw new Exception(s"invalid envDef value ${other}")
            }.toVector
          case other => throw new Exception(s"invalid version ${other}")
        }
    )
  }
}

case object ShellCommandRequirement extends Requirement with HintSchema {
  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = this
}

case class ResourceRequirement(coresMin: Option[CwlValue] = None,
                               coresMax: Option[CwlValue] = None,
                               ramMin: Option[CwlValue] = None,
                               ramMax: Option[CwlValue] = None,
                               tmpdirMin: Option[CwlValue] = None,
                               tmpdirMax: Option[CwlValue] = None,
                               outdirMin: Option[CwlValue] = None,
                               outdirMax: Option[CwlValue] = None)
    extends Requirement {

  assert(Requirement.lte(coresMin, coresMax))
  assert(Requirement.lte(ramMin, ramMax))
  assert(Requirement.lte(tmpdirMin, tmpdirMax))
  assert(Requirement.lte(outdirMin, outdirMax))

  /**
    * Merge another ResourceRequirement by replacing any `None`
    * value in this object with the corresponding value in `other`.
    * If there is a mismatch between any pair of (min,max) values,
    * the min value takes priority - e.g. if this.coresMin = 2
    * and that.coresMax = 1, in the new ResourceRequirement
    * coresMin = coresMax = 2.
    * @param that the ResourceRequirement to merge
    * @return a new ResourceRequirement
    */
  def merge(that: ResourceRequirement): ResourceRequirement = {
    val (minCores, maxCores) = Requirement.updateMinMax(
        coresMin.orElse(that.coresMin),
        coresMax.orElse(that.coresMax),
        ResourceRequirement.DefaultCoresMin
    )
    val (minRam, maxRam) = Requirement.updateMinMax(
        ramMin.orElse(that.ramMin),
        ramMax.orElse(that.ramMax),
        ResourceRequirement.DefaultRamMin
    )
    val (minTmpdir, maxTmpdir) = Requirement.updateMinMax(
        tmpdirMin.orElse(that.tmpdirMin),
        tmpdirMax.orElse(that.tmpdirMax),
        ResourceRequirement.DefaultTmpdirMin
    )
    val (minOutdir, maxOutdir) = Requirement.updateMinMax(
        outdirMin.orElse(that.outdirMin),
        outdirMax.orElse(that.outdirMax),
        ResourceRequirement.DefaultOutdirMin
    )
    ResourceRequirement(
        minCores,
        maxCores,
        minRam,
        maxRam,
        minTmpdir,
        maxTmpdir,
        minOutdir,
        maxOutdir
    )
  }

  /**
    * Merges this ResourceRequirement with the default values.
    */
  def complete: ResourceRequirement = {
    merge(ResourceRequirement.default)
  }
}

object ResourceRequirement extends HintSchema {
  val DefaultCoresMin = 1
  val DefaultRamMin = 256
  val DefaultTmpdirMin = 1024
  val DefaultOutdirMin = 1024

  def apply(req: ResourceRequirementImpl,
            schemaDefs: Map[String, CwlSchema]): ResourceRequirement = {
    ResourceRequirement(
        translateOptionalObject(req.getCoresMin).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getCoresMax).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getRamMin).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getRamMax).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getTmpdirMin).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getTmpdirMax).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getOutdirMin).map(CwlValue(_, schemaDefs)),
        translateOptionalObject(req.getOutdirMax).map(CwlValue(_, schemaDefs))
    )
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    ResourceRequirement(
        coresMin = hint.get("coresMin").map(CwlValue(_, schemaDefs)),
        coresMax = hint.get("coresMax").map(CwlValue(_, schemaDefs)),
        ramMin = hint.get("ramMin").map(CwlValue(_, schemaDefs)),
        ramMax = hint.get("ramMin").map(CwlValue(_, schemaDefs)),
        tmpdirMin = hint.get("tmpdirMin").map(CwlValue(_, schemaDefs)),
        tmpdirMax = hint.get("tmpdirMax").map(CwlValue(_, schemaDefs)),
        outdirMin = hint.get("coresMin").map(CwlValue(_, schemaDefs)),
        outdirMax = hint.get("outdirMax").map(CwlValue(_, schemaDefs))
    )
  }

  lazy val empty: ResourceRequirement = ResourceRequirement()

  /**
    * ResourceRequirement with default values as
    * @return
    */
  lazy val default: ResourceRequirement = ResourceRequirement(
      coresMin = Some(LongValue(DefaultCoresMin)),
      ramMin = Some(LongValue(DefaultRamMin)),
      tmpdirMin = Some(LongValue(DefaultTmpdirMin)),
      outdirMin = Some(LongValue(DefaultOutdirMin))
  )
}

case class WorkReuseRequirement(enable: CwlValue) extends Requirement

object WorkReuseRequirement extends HintSchema {
  def apply(req: WorkReuseImpl, schemaDefs: Map[String, CwlSchema]): WorkReuseRequirement = {
    WorkReuseRequirement(CwlValue(req.getEnableReuse, schemaDefs))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    WorkReuseRequirement(enable = CwlValue(hint("enableReuse"), schemaDefs))
  }
}

case class NetworkAccessRequirement(allow: CwlValue) extends Requirement

object NetworkAccessRequirement extends HintSchema {
  def apply(req: NetworkAccessImpl,
            schemaDefs: Map[String, CwlSchema]): NetworkAccessRequirement = {
    NetworkAccessRequirement(CwlValue(req.getNetworkAccess, schemaDefs))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    NetworkAccessRequirement(allow = CwlValue(hint("networkAccess"), schemaDefs))
  }
}

case class InplaceUpdateRequirement(allow: Boolean) extends Requirement

object InplaceUpdateRequirement extends HintSchema {
  def apply(req: InplaceUpdateRequirementImpl): InplaceUpdateRequirement = {
    InplaceUpdateRequirement(req.getInplaceUpdate)
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    InplaceUpdateRequirement(allow = hint("inplaceUpdate") match {
      case b: Boolean => b
      case other      => throw new Exception(s"invalid inplaceUpdate value ${other}")
    })
  }
}

case class ToolTimeLimitRequirement(timeLimit: CwlValue) extends Requirement

object ToolTimeLimitRequirement extends HintSchema {
  def apply(req: ToolTimeLimitImpl,
            schemaDefs: Map[String, CwlSchema]): ToolTimeLimitRequirement = {
    ToolTimeLimitRequirement(CwlValue(req.getTimelimit, schemaDefs))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    ToolTimeLimitRequirement(timeLimit = CwlValue(hint("timelimit"), schemaDefs))
  }
}

object RequirementUtils {
  def getJsRequirements(requirements: Vector[Requirement]): (Boolean, Option[String]) = {
    requirements.collect {
      case req: InlineJavascriptRequirement => req
    } match {
      case Vector()    => (false, None)
      case Vector(req) => (true, req.expressionLib)
      case _ =>
        throw new Exception("found multiple InlineJavascriptRequirements")
    }
  }

  def getSchemaDefs(requirements: Vector[Requirement]): Map[String, CwlSchema] = {
    requirements
      .collect {
        case req: SchemaDefRequirement => req.asMap
      }
      .flatten
      .toMap
  }
}
