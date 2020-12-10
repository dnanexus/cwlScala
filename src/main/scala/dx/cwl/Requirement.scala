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
  * One of the requirements defined in the CWL spec.
  * https://www.commonwl.org/v1.2/CommandLineTool.html#InlineJavascriptRequirement
  */
sealed trait Requirement

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

object InlineJavascriptRequirement {
  def apply(req: InlineJavascriptRequirementImpl): InlineJavascriptRequirement = {
    val exprLib = translateOptionalArray(req.getExpressionLib).asOption
      .map(_.iterator.map(translateString).mkString("\n"))
    InlineJavascriptRequirement(exprLib)
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

object LoadListingRequirement {
  def apply(req: LoadListingRequirementImpl): LoadListingRequirement = {
    LoadListingRequirement(translateOptional(req.getLoadListing).map(LoadListing.from))
  }
}

case class DockerRequirement(pullName: Option[String],
                             loadUri: Option[String],
                             importUri: Option[String],
                             dockerfile: Option[String],
                             imageId: Option[String],
                             outputDirectory: Option[String])
    extends Requirement

object DockerRequirement {
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
}

case class SoftwarePackage(name: String, version: Vector[String], specs: Vector[String])

object SoftwarePackage {
  def apply(pkg: SoftwarePackageImpl): SoftwarePackage = {
    SoftwarePackage(pkg.getPackage,
                    translateOptionalArray(pkg.getVersion).map(translateString),
                    translateOptionalArray(pkg.getSpecs).map(translateString))
  }
}

case class SoftwareRequirement(packages: Vector[SoftwarePackage]) extends Requirement

object SoftwareRequirement {
  def apply(req: SoftwareRequirementImpl): SoftwareRequirement = {
    SoftwareRequirement(
        req.getPackages.asScala.map {
          case pkg: SoftwarePackageImpl => SoftwarePackage(pkg)
          case other =>
            throw new RuntimeException(s"unexpected SoftwarePackage value ${other}")
        }.toVector
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
}

case class InitialWorkDirRequirement(listing: Vector[InitialWorkDirEntry]) extends Requirement

object InitialWorkDirRequirement {
  def apply(req: InitialWorkDirRequirementImpl,
            schemaDefs: Map[String, CwlSchema]): InitialWorkDirRequirement = {
    InitialWorkDirRequirement(translateArray(req.getListing).map {
      case entry: DirentImpl => DirInitialWorkDirEntry(entry, schemaDefs)
      case value             => ValueInitialWorkDirEntry(CwlValue(value, schemaDefs))
    })
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

object EnvVarRequirement {
  def apply(req: EnvVarRequirementImpl, schemaDefs: Map[String, CwlSchema]): EnvVarRequirement = {
    EnvVarRequirement(req.getEnvDef.asScala.map {
      case env: EnvironmentDefImpl => EnvironmentDefinition(env, schemaDefs)
      case other =>
        throw new RuntimeException(s"unexpected EnvironmentDef value ${other}")
    }.toVector)
  }
}

case object ShellCommandRequirement extends Requirement

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

object ResourceRequirement {
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

object WorkReuseRequirement {
  def apply(req: WorkReuseImpl, schemaDefs: Map[String, CwlSchema]): WorkReuseRequirement = {
    WorkReuseRequirement(CwlValue(req.getEnableReuse, schemaDefs))
  }
}

case class NetworkAccessRequirement(allow: CwlValue) extends Requirement

object NetworkAccessRequirement {
  def apply(req: NetworkAccessImpl,
            schemaDefs: Map[String, CwlSchema]): NetworkAccessRequirement = {
    NetworkAccessRequirement(CwlValue(req.getNetworkAccess, schemaDefs))
  }
}

case class InplaceUpdateRequirement(allow: Boolean) extends Requirement

object InplaceUpdateRequirement {
  def apply(req: InplaceUpdateRequirementImpl): InplaceUpdateRequirement = {
    InplaceUpdateRequirement(req.getInplaceUpdate)
  }
}

case class ToolTimeLimitRequirement(timeLimit: CwlValue) extends Requirement

object ToolTimeLimitRequirement {
  def apply(req: ToolTimeLimitImpl,
            schemaDefs: Map[String, CwlSchema]): ToolTimeLimitRequirement = {
    ToolTimeLimitRequirement(CwlValue(req.getTimelimit, schemaDefs))
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
