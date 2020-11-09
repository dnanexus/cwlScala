import sbt.Keys._
import sbtassembly.AssemblyPlugin.autoImport._

scalaVersion := "2.13.2"
name := "cwlScala"
import com.typesafe.config._
val confPath =
  Option(System.getProperty("config.file"))
    .getOrElse("src/main/resources/application.conf")
val conf = ConfigFactory.parseFile(new File(confPath)).resolve()
version := conf.getString("cwlScala.version")
organization := "com.dnanexus"
developers := List(
    Developer(
        "jdidion",
        "jdidion",
        "jdidion@dnanexus.com",
        url("https://github.com/dnanexus")
    )
)
homepage := Some(url("https://github.com/dnanexus/cwlScala"))
scmInfo := Some(
    ScmInfo(
        url("https://github.com/dnanexus/cwlScala"),
        "git@github.com:dnanexus/cwlScala"
    )
)
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
publishMavenStyle := true

val root = project.in(file("."))

// disable publish with scala version, otherwise artifact name will include scala version
// e.g cwlScala_2.11
crossPaths := false

// add sonatype repository settings
// snapshot versions publish to sonatype snapshot repository
// other versions publish to sonatype staging repository
publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
)

// reduce the maximum number of errors shown by the Scala compiler
maxErrors := 20

//coverageEnabled := true

javacOptions ++= Seq("-Xlint:deprecation")

// Show deprecation warnings
scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-explaintypes",
    "-encoding",
    "UTF-8",
    "-Xlint:constant",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:nullary-override",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Ywarn-dead-code",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:privates",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:imports", // warns about every unused import on every command.
    "-Xfatal-warnings" // makes those warnings fatal.
)

assemblyJarName in assembly := "cwlScala.jar"
logLevel in assembly := Level.Info

val typesafeVersion = "1.3.3"
val sprayVersion = "1.3.5"
val scalacticVersion = "3.1.1"
val scalatestVersion = "3.1.1"
val yamlVersion = "1.24"
val rhinoVersion = "1.7.13"
val antlr4Version = "4.8"
val junitVersion = "4.12"

libraryDependencies ++= Seq(
    "com.typesafe" % "config" % typesafeVersion,
    "io.spray" %% "spray-json" % sprayVersion,
    // cwljava dependencies
    "org.yaml" % "snakeyaml" % yamlVersion,
    // rhino dependencies
    "org.mozilla" % "rhino" % rhinoVersion,
    // antlr4 dependencies
    "org.antlr" % "antlr4" % antlr4Version,
    //---------- Test libraries -------------------//
    "junit" % "junit" % junitVersion % Test,
    "org.scalatest" % "scalatest_2.13" % scalatestVersion % Test
)

// Add Java sources
Compile / unmanagedSourceDirectories += baseDirectory.value / "cwljava" / "src" / "main" / "java"

assemblyMergeStrategy in assembly := {
  {
    case PathList("javax", "xml", xs @ _*)               => MergeStrategy.first
    case PathList("org", "w3c", "dom", "TypeInfo.class") => MergeStrategy.first
    case PathList("META_INF", xs @ _*) =>
      xs map { _.toLowerCase } match {
        case "manifest.mf" :: Nil | "index.list" :: Nil | "dependencies" :: Nil =>
          MergeStrategy.discard
        case _ => MergeStrategy.last
      }
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
}

// If an exception is thrown during tests, show the full
// stack trace, by adding the "-oF" option to the list.
//

Test / testOptions += Tests.Argument("-oF")

Test / parallelExecution := false

// comment out this line to enable tests in assembly
test in assembly := {}

// scalafmt
scalafmtConfig := root.base / ".scalafmt.conf"
// Coverage
//
// sbt clean coverage test
// sbt coverageReport

// To turn it off do:
// sbt coverageOff

// Ignore code parts that cannot be checked in the unit
// test environment
//coverageExcludedPackages :=

// exclude Java sources from scaladoc
scalacOptions in (Compile, doc) ++= Seq("-no-java-comments", "-no-link-warnings")
