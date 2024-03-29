package dx.cwl

import dx.cwl.Utils._
import dx.util.CollectionUtils.IterableExtensions
import org.w3id.cwl.cwl1_2.{
  DirentImpl,
  DockerRequirementImpl,
  EnvVarRequirementImpl,
  EnvironmentDefImpl,
  InitialWorkDirRequirementImpl,
  InlineJavascriptRequirementImpl,
  InplaceUpdateRequirementImpl,
  LoadListingEnum,
  LoadListingRequirementImpl,
  MultipleInputFeatureRequirementImpl,
  NetworkAccessImpl,
  ProcessRequirement,
  ResourceRequirementImpl,
  ScatterFeatureRequirementImpl,
  SchemaDefRequirementImpl,
  ShellCommandRequirementImpl,
  SoftwarePackageImpl,
  SoftwareRequirementImpl,
  StepInputExpressionRequirementImpl,
  SubworkflowFeatureRequirementImpl,
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
  val className: String = {
    getClass.getSimpleName match {
      case name if name.endsWith("$") => name.dropRight(1)
      case name                       => name
    }
  }

  def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint
}

case class GenericHint(attributes: Map[String, Any]) extends Hint

/**
  * One of the requirements defined in the CWL spec.
  * https://www.commonwl.org/v1.2/CommandLineTool.html#InlineJavascriptRequirement
  * https://www.commonwl.org/v1.2/Workflow.html#InlineJavascriptRequirement
  */
sealed trait Requirement extends Hint

object Requirement {
  def apply(requirement: ProcessRequirement, schemaDefs: Map[String, CwlSchema]): Requirement = {
    requirement match {
      case req: InlineJavascriptRequirementImpl   => InlineJavascriptRequirement(req)
      case req: SchemaDefRequirementImpl          => SchemaDefRequirement(req, Map.empty)
      case req: LoadListingRequirementImpl        => LoadListingRequirement(req)
      case req: DockerRequirementImpl             => DockerRequirement(req)
      case req: SoftwareRequirementImpl           => SoftwareRequirement(req)
      case req: InitialWorkDirRequirementImpl     => InitialWorkDirRequirement(req, schemaDefs)
      case req: EnvVarRequirementImpl             => EnvVarRequirement(req, schemaDefs)
      case _: ShellCommandRequirementImpl         => ShellCommandRequirement
      case req: ResourceRequirementImpl           => ResourceRequirement(req, schemaDefs)
      case req: WorkReuseImpl                     => WorkReuse(req, schemaDefs)
      case req: NetworkAccessImpl                 => NetworkAccess(req, schemaDefs)
      case req: InplaceUpdateRequirementImpl      => InplaceUpdateRequirement(req)
      case req: ToolTimeLimitImpl                 => ToolTimeLimit(req, schemaDefs)
      case _: SubworkflowFeatureRequirementImpl   => SubworkflowFeatureRequirement
      case _: ScatterFeatureRequirementImpl       => ScatterFeatureRequirement
      case _: MultipleInputFeatureRequirementImpl => MultipleInputFeatureRequirement
      case _: StepInputExpressionRequirementImpl  => StepInputExpressionRequirement
      case _ =>
        throw new RuntimeException(s"unsupported requirement value ${requirement}")
    }
  }

  def applyRequirements(
      requirements: java.util.Optional[java.util.List[Object]],
      schemaDefs: Map[String, CwlSchema] = Map.empty
  ): (Vector[Requirement], Map[String, CwlSchema]) = {
    val reqArray = translateOptionalArray(requirements)
    // first we have to collect the SchemaDefRequirements and parse them
    // in dependency order - since one schema may refer to another
    val allSchemaDefs = schemaDefs ++ SchemaDefRequirement.translateTypes(
        reqArray.collect {
          case req: SchemaDefRequirementImpl => req
        },
        schemaDefs
    )
    val cwlRequirements = reqArray.flatMap {
      case _: SchemaDefRequirementImpl => None
      case req: ProcessRequirement     => Some(Requirement(req, allSchemaDefs))
      case req =>
        throw new RuntimeException(s"unexpected requirement value ${req}")
    }
    val allRequirements = if (allSchemaDefs.nonEmpty) {
      cwlRequirements :+ SchemaDefRequirement(allSchemaDefs.values.toVector)
    } else {
      cwlRequirements
    }
    (allRequirements, allSchemaDefs)
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
      WorkReuse,
      NetworkAccess,
      InplaceUpdateRequirement,
      ToolTimeLimit
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

  def applyHints(requirements: java.util.Optional[java.util.List[Object]],
                 schemaDefs: Map[String, CwlSchema] = Map.empty,
                 hintSchemas: Map[String, HintSchema] = Map.empty): Vector[Hint] = {
    translateOptionalArray(requirements).map {
      case attrs: java.util.Map[_, _] =>
        val rawHints = attrs.asScala.toMap match {
          case hints: Map[_, _] => toStringAnyMap(hints)
          case other            => throw new Exception(s"invalid hints ${other.getClass}")
        }
        Requirement.apply(rawHints, schemaDefs, hintSchemas)
      case other =>
        throw new Exception(s"unexpected hints value ${other}")
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
  typeDefinitions.foreach { typeDef =>
    if (typeDef.frag.isEmpty) {
      throw new Exception(s"schema name must be defined in SchemaDefRequirement ${typeDef}")
    }
  }

  def asMap: Map[String, CwlSchema] = {
    typeDefinitions.map(schema => schema.frag -> schema)
  }.toMap
}

object SchemaDefRequirement {
  def translateTypes(reqs: Vector[SchemaDefRequirementImpl],
                     schemaDefs: Map[String, CwlSchema]): Map[String, CwlSchema] = {
    CwlSchema.translateAll(reqs.flatMap(_.getTypes.asScala), schemaDefs)
  }

  def apply(req: SchemaDefRequirementImpl,
            schemaDefs: Map[String, CwlSchema]): SchemaDefRequirement = {
    SchemaDefRequirement(translateTypes(Vector(req), schemaDefs).values.toVector)
  }
}

case class LoadListingRequirement(value: Option[LoadListing.LoadListing]) extends Requirement

object LoadListingRequirement extends HintSchema {
  def apply(req: LoadListingRequirementImpl): LoadListingRequirement = {
    LoadListingRequirement(translateOptional(req.getLoadListing).map(LoadListing.from))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    LoadListingRequirement(value = hint
      .get("loadListing")
      .map(name => LoadListing.from(LoadListingEnum.fromDocumentVal(name.toString)))
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
            case versions: java.util.Collection[_] => versions.asScala.map(_.toString).toVector
            case other                             => throw new Exception(s"invalid version ${other}")
          }
          .getOrElse(Vector.empty),
        specs = hint
          .get("specs")
          .map {
            case versions: java.util.Collection[_] => versions.asScala.map(_.toString).toVector
            case other                             => throw new Exception(s"invalid version ${other}")
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
          case packages: java.util.Collection[_] =>
            packages.asScala.map {
              case pkg: java.util.Map[_, _] => SoftwarePackage(toStringAnyMap(pkg.asScala.toMap))
            }.toVector
          case other =>
            throw new RuntimeException(s"unexpected package value ${other}")
        }
    )
  }
}

sealed trait InitialWorkDirEntry
case class ValueInitialWorkDirEntry(value: CwlValue) extends InitialWorkDirEntry
case class DirInitialWorkDirEntry(entry: CwlValue, entryName: Option[CwlValue], writable: Boolean)
    extends InitialWorkDirEntry

object DirInitialWorkDirEntry {
  def apply(dirent: DirentImpl, schemaDefs: Map[String, CwlSchema]): DirInitialWorkDirEntry = {
    val entry = CwlValue(dirent.getEntry, schemaDefs)
    val entryName = translateOptionalObject(dirent.getEntryname).map(CwlValue(_, schemaDefs))
    val writable = translateOptional[java.lang.Boolean](dirent.getWritable).exists(_.booleanValue())
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
        writable = dirent.get("writable") match {
          case Some(s: String)  => s.toBoolean
          case Some(b: Boolean) => b
          case None             => false
          case other            => throw new Exception(s"invalid writable value ${other}")
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
          case entry: java.util.Collection[_] =>
            entry.asScala.map {
              case m: java.util.Map[_, _] =>
                DirInitialWorkDirEntry.apply(toStringAnyMap(m.asScala.toMap), schemaDefs)
              case other => throw new Exception(s"invalid entry value ${other}")
            }.toVector
          case value =>
            Vector(ValueInitialWorkDirEntry(CwlValue(value, schemaDefs)))
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
        environmentDefinitions = hint
          .getOrElse("envDef", throw new Exception("missing required attribute 'envDef'")) match {
          case defs: java.util.Map[_, _] =>
            toStringAnyMap(defs.asScala.toMap).map {
              case (name, value) => EnvironmentDefinition(name, CwlValue(value, schemaDefs))
            }.toVector
          case defs: java.util.Collection[_] =>
            defs.asScala.map {
              case d: java.util.Map[_, _] =>
                val dStrAny = toStringAnyMap(d.asScala.toMap)
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
  def merge(that: ResourceRequirement = ResourceRequirement.default): ResourceRequirement = {
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

case class WorkReuse(enable: CwlValue) extends Requirement

object WorkReuse extends HintSchema {
  def apply(req: WorkReuseImpl, schemaDefs: Map[String, CwlSchema]): WorkReuse = {
    WorkReuse(CwlValue(req.getEnableReuse, schemaDefs))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    WorkReuse(enable = CwlValue(hint("enableReuse"), schemaDefs))
  }
}

case class NetworkAccess(allow: CwlValue) extends Requirement

object NetworkAccess extends HintSchema {
  def apply(req: NetworkAccessImpl, schemaDefs: Map[String, CwlSchema]): NetworkAccess = {
    NetworkAccess(CwlValue(req.getNetworkAccess, schemaDefs))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    NetworkAccess(allow = CwlValue(hint("networkAccess"), schemaDefs))
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

case class ToolTimeLimit(timeLimit: CwlValue) extends Requirement

object ToolTimeLimit extends HintSchema {
  def apply(req: ToolTimeLimitImpl, schemaDefs: Map[String, CwlSchema]): ToolTimeLimit = {
    ToolTimeLimit(CwlValue(req.getTimelimit, schemaDefs))
  }

  override def apply(hint: Map[String, Any], schemaDefs: Map[String, CwlSchema]): Hint = {
    ToolTimeLimit(timeLimit = CwlValue(hint("timelimit"), schemaDefs))
  }
}

case object SubworkflowFeatureRequirement extends Requirement
case object ScatterFeatureRequirement extends Requirement
case object MultipleInputFeatureRequirement extends Requirement
case object StepInputExpressionRequirement extends Requirement

object HintUtils {

  /**
    * Gets the JavaScript hint from a list of Hints. If there are multiple
    * JavaScript hints, the *last* one is selected.
    * @param hints Vector of Hints
    * @return
    */
  def getJsHint(hints: Vector[Hint]): (Boolean, Option[String]) = {
    hints.reverseIterator.collectFirst {
      case req: InlineJavascriptRequirement => req
    } match {
      case Some(req) => (true, req.expressionLib)
      case _         => (false, None)
    }
  }

  /**
    * Gets the SchemaDef hints from a list of hints. If there are
    * multiple SchemaDef hints, all are merged, with later ones
    * overriding earlier ones.
    * @param hints Vector of Hints
    * @return
    */
  def getSchemaDefs(hints: Vector[Hint]): Map[String, CwlSchema] = {
    hints
      .collect {
        case req: SchemaDefRequirement => req.asMap
      }
      .flatten
      .toMap
  }
}
