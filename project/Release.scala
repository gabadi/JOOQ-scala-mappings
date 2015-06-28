import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
import sbtrelease.ReleasePlugin.autoImport._

object Release {

  lazy val releaseSettings = ReleasePlugin.projectSettings ++ Seq(
    releaseVersionBump := sbtrelease.Version.Bump.Bugfix,
    publishMavenStyle := true,
    publishArtifact in Test := false,
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
    ),
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
  )

}