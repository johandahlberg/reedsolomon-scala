enablePlugins(JavaAppPackaging)

import Dependencies._

ThisBuild / scalaVersion := "2.13.0"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "se.scilifelab"
ThisBuild / organizationName := "scilifelab"

// TODO It would probably be smart to break out the performance
// tests to their own project, so that e.g. the parallel test execution
// did not have to be disabled for the entire project.

lazy val sonatype = "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
logBuffered := false

lazy val root = (project in file("."))
  .settings(
    name := "reedsolomon-scala",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaCheck % Test,
      scalaMeter % Test
    ),
    resolvers += sonatype
  )

testOptions in Test += Tests.Argument(
  TestFrameworks.ScalaCheck,
  "-verbosity",
  "2"
)
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
