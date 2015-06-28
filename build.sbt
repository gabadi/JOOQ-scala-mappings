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

pomExtra in Global := {
  <url>(your project URL)</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com/gabadi/JOOQ-scala-mappings</connection>
      <developerConnection>scm:git:git@github.com:gabadi/JOOQ-scala-mappings</developerConnection>
      <url>github.com/gabadi/JOOQ-scala-mappings</url>
    </scm>
    <developers>
      <developer>
        <id>1</id>
        <name>gabriel.d.abadi@gmail.com</name>
        <url>https://github.com/gabadi</url>
      </developer>
    </developers>
}