import sbt.Keys._
import sbtassembly.AssemblyPlugin.autoImport._

scalaVersion := "2.13.2"
name := "dxCWL"
//version := "0.1.0"
import com.typesafe.config._
val confPath =
  Option(System.getProperty("config.file"))
    .getOrElse("src/main/resources/application.conf")
val conf = ConfigFactory.parseFile(new File(confPath)).resolve()
version := conf.getString("dxCWL.version")
organization := "com.dnanexus"
developers := List(
  Developer(
    "jdidion",
    "jdidion",
    "jdidion@dnanexus.com",
    url("https://github.com/dnanexus-rnd")
  )
)
homepage := Some(url("https://github.com/dnanexus-rnd/dxCWL"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/dnanexus-rnd/dxCWL"),
    "git@github.com:dnanexus-rnd/dxCWL"
  )
)
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
publishMavenStyle := true

val root = project.in(file("."))

// disable publish with scala version, otherwise artifact name will include scala version
// e.g dxCWL_2.11
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

assemblyJarName in assembly := "dxCWL.jar"
logLevel in assembly := Level.Info

val typesafeVersion = "1.3.3"
val sprayVersion = "1.3.5"
val scalacticVersion = "3.1.1"
val scalatestVersion = "3.1.1"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % typesafeVersion,
  "io.spray" %% "spray-json" % sprayVersion,
  //---------- Test libraries -------------------//
  "org.scalactic" % "scalactic_2.13" % scalacticVersion,
  "org.scalatest" % "scalatest_2.13" % scalatestVersion % "test"
)

// If an exception is thrown during tests, show the full
// stack trace, by adding the "-oF" option to the list.
//

// exclude the native tests, they are slow.
// to do this from the command line:
// sbt testOnly -- -l native
//
// comment out this line in order to allow native
// tests
// Test / testOptions += Tests.Argument("-l", "native")
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
