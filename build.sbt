import Merging.customMergeStrategy
import sbt.Keys._
import sbt.{ThisBuild, project}
import sbtassembly.AssemblyPlugin.autoImport._
import com.typesafe.config._
import sbtghpackages.GitHubPackagesPlugin.autoImport.githubOwner

name := "cwlScala"

def getVersion: String = {
  val confPath =
    Option(System.getProperty("config.file")).getOrElse("src/main/resources/application.conf")
  val conf = ConfigFactory.parseFile(new File(confPath)).resolve()
  conf.getString("cwlScala.version")
}

ThisBuild / organization := "com.dnanexus"
ThisBuild / scalaVersion := "2.13.7"
ThisBuild / developers := List(
    Developer(
        "jdidion",
        "jdidion",
        "jdidion@dnanexus.com",
        url("https://github.com/dnanexus")
    )
)
ThisBuild / homepage := Some(url("https://github.com/dnanexus/cwlScala"))
ThisBuild / scmInfo := Some(
    ScmInfo(
        url("https://github.com/dnanexus/cwlScala"),
        "git@github.com:dnanexus/cwlScala"
    )
)
ThisBuild / licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val cwljava = project
  .in(file("cwljava"))
  .settings(
      settings,
      libraryDependencies ++= Seq(
          dependencies.snakeyaml,
          dependencies.junit
      )
  )
  .disablePlugins(AssemblyPlugin)

lazy val root = project
  .in(file("."))
  .settings(
      name := "cwlScala",
      version := getVersion,
      settings,
      assemblySettings,
      libraryDependencies ++= Seq(
          dependencies.dxCommon,
          dependencies.dxYaml,
          dependencies.typesafe,
          dependencies.spray,
          dependencies.rhino,
          dependencies.antlr,
          dependencies.scalatest,
          dependencies.snakeyaml,
          dependencies.junit
      ),
      assembly / assemblyJarName := "cwlScala.jar"
  )
  .aggregate(cwljava)
  .dependsOn(cwljava)

lazy val dependencies = new {
  val dxCommonVersion = "0.10.0"
  val dxYamlVersion = "0.1.0"
  val typesafeVersion = "1.4.1"
  val sprayVersion = "1.3.6"
  val scalatestVersion = "3.2.9"
  val yamlVersion = "2.3"
  val rhinoVersion = "1.7.13"
  val antlr4Version = "4.9.3"
  val junitVersion = "4.13.2"

  val dxCommon = "com.dnanexus" % "dxcommon" % dxCommonVersion
  val dxYaml = "com.dnanexus" % "dxyaml" % dxYamlVersion
  val typesafe = "com.typesafe" % "config" % typesafeVersion
  val spray = "io.spray" %% "spray-json" % sprayVersion
  // cwljava dependencies
  val snakeyaml = "org.snakeyaml" % "snakeyaml-engine" % yamlVersion
  // rhino dependencies
  val rhino = "org.mozilla" % "rhino" % rhinoVersion
  // antlr4 dependencies
  val antlr = "org.antlr" % "antlr4" % antlr4Version
  //---------- Test libraries -------------------//
  val junit = "junit" % "junit" % junitVersion % Test
  val scalatest = "org.scalatest" % "scalatest_2.13" % scalatestVersion % Test
}

val githubDxScalaResolver = Resolver.githubPackages("dnanexus", "dxScala")
val githubCwlScalaResolver = Resolver.githubPackages("dnanexus", "cwlScala")
resolvers ++= Vector(githubCwlScalaResolver, githubDxScalaResolver)

val releaseTarget = Option(System.getProperty("releaseTarget")).getOrElse("github")

lazy val settings = Seq(
    scalacOptions ++= compilerOptions,
    Compile / doc / scalacOptions ++= Seq("-no-java-comments", "-no-link-warnings"),
    javacOptions ++= Seq("-Xlint:deprecation", "-source", "1.8", "-target", "1.8"),
    // reduce the maximum number of errors shown by the Scala compiler
    maxErrors := 20,
    // disable publish with scala version, otherwise artifact name will include scala version e.g wdlTools_2.11
    crossPaths := false,
    publishMavenStyle := true,
    // scalafmt
    scalafmtConfig := baseDirectory.value / ".scalafmt.conf",
    // add sonatype repository settings
    // snapshot versions publish to sonatype snapshot repository
    // other versions publish to sonatype staging repository
    publishTo := Some(
        if (isSnapshot.value || releaseTarget == "github") {
          githubCwlScalaResolver
        } else {
          Opts.resolver.sonatypeStaging
        }
    ),
    githubOwner := "dnanexus",
    githubRepository := "cwlScala",
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
    Test / testOptions += Tests.Argument("-oF"),
    Test / parallelExecution := false

    // Coverage
    //
    // sbt clean coverage test
    // sbt coverageReport
    // To turn it off do:
    // sbt coverageOff
    //coverageEnabled := true
    // Ignore code parts that cannot be checked in the unit
    // test environment
    //coverageExcludedPackages := "dxWDL.Main;dxWDL.compiler.DxNI;dxWDL.compiler.DxObjectDirectory;dxWDL.compiler.Native"
)

// Show deprecation warnings
val compilerOptions = Seq(
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

// Assembly
lazy val assemblySettings = Seq(
    assembly / logLevel := Level.Info,
    // comment out this line to enable tests in assembly
    assembly / test := {},
    assembly / assemblyMergeStrategy := {
      {
        case PathList("javax", "xml", _ @_*)                 => MergeStrategy.first
        case PathList("org", "w3c", "dom", "TypeInfo.class") => MergeStrategy.first
        case PathList("META_INF", xs @ _*) =>
          xs map { _.toLowerCase } match {
            case "manifest.mf" :: Nil | "index.list" :: Nil | "dependencies" :: Nil =>
              MergeStrategy.discard
            case _ => MergeStrategy.last
          }
        case x =>
          customMergeStrategy.value(x)
      }
    }
)
