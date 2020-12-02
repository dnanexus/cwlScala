package dx.cwl

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
