package dx.cwl

import dx.cwl.Utils._
import org.w3id.cwl.cwl1_2.{
  CommandInputSchema,
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

case class InitialWorkDirRequirement(listing: Vector[CwlValue]) extends Requirement

object InitialWorkDirRequirement {
  def apply(req: InitialWorkDirRequirementImpl,
            schemaDefs: Map[String, CwlSchema]): InitialWorkDirRequirement = {
    InitialWorkDirRequirement(translateArray(req.getListing).map(CwlValue(_, schemaDefs)))
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

case class ResourceRequirement(coresMin: Option[CwlValue],
                               coresMax: Option[CwlValue],
                               ramMin: Option[CwlValue],
                               ramMax: Option[CwlValue],
                               tmpdirMin: Option[CwlValue],
                               tmpdirMax: Option[CwlValue],
                               outdirMin: Option[CwlValue],
                               outdirMax: Option[CwlValue])
    extends Requirement

object ResourceRequirement {
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
