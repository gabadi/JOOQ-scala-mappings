import ReleaseTransformations._

name := "JOOQ-scala-mappings"

organization := "com.github.gabadi.scalajooq"

version := "1.0"

scalaVersion := Dependencies.scalaV

lazy val macros = project.in(file("."))
  .settings(Dependencies.settings : _*)
  .settings(TestSettings.testSettings :_*)

releaseVersionBump := sbtrelease.Version.Bump.Major

//releasePublishArtifactsAction := PgpKeys.publishSigned.value

publishMavenStyle := true

publishArtifact in Test := false

scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.7",
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code"
)

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  pushChanges
)