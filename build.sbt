import MimaSettings.mimaSettings

name := "zio-ubench"
organization := "dev.koubic"
version := scala.io.Source.fromFile(s"${baseDirectory.value}/version").mkString("").trim

val scalaVersions = Seq(
  "2.13.5",
  "2.12.13",
  "2.11.12",
  "3.0.0-RC3"
)
scalaVersion := scalaVersions.head
crossScalaVersions := scalaVersions

versionScheme := Some("semver-spec")

scalacOptions ++=
  Seq("-unchecked", "-deprecation", "-feature", "-Xlint") ++
    Seq("-language:higherKinds").filter(_ =>
      CrossVersion.partialVersion(scalaVersion.value).get match {
        case (2, maj) if maj <= 12 => true
        case _ => false
      }
    )

// Dependencies
val zioVersion = "1.0.7"

libraryDependencies += "dev.zio" %% "zio" % zioVersion

// Test settings
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-test" % zioVersion % "test->default",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")


// scalafix and scalafmt settings
semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)
scalacOptions ++= Seq("-Ywarn-unused-import").filter(_ =>
  CrossVersion.partialVersion(scalaVersion.value).get match {
    case (2, 13) => false
    case _ => true
  }
)

mimaSettings(failOnProblem = false)

