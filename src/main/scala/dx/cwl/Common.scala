package dx.cwl

import org.w3id.cwl.cwl1_2.{LoadListingEnum, SecondaryFileSchemaImpl}

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
}

// https://www.commonwl.org/v1.2/CommandLineTool.html#SecondaryFileSchema
case class SecondaryFile(pattern: CwlValue, required: CwlValue)

object SecondaryFile {
  def apply(secondaryFile: SecondaryFileSchemaImpl,
            schemaDefs: Map[String, CwlSchema]): SecondaryFile = {
    SecondaryFile(CwlValue(secondaryFile.getPattern, schemaDefs),
                  CwlValue(secondaryFile.getRequired, schemaDefs))
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
