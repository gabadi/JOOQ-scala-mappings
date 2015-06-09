import sbt.Keys._
import sbt._

object Release {
  /*
publishTo <<= (version in ThisBuild) {
  v: String =>
    val nexus = "http://nexus.despegar.it:8080/nexus/content/repositories/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("Despegar Snapshots" at nexus + "snapshots/")
    else
      Some("Despegar Releases" at nexus + "releases/")
}
*/
/*
  lazy val distHack = TaskKey[File]("dist-hack", "Hack to publish dist")

  lazy val releaseSettings = ReleasePlugin.releaseSettings ++ Seq(
    publishTo <<= (version in ThisBuild) {
      v: String =>
        val nexus = "http://nexus.despegar.it:8080/nexus/content/repositories/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("Despegar Snapshots" at nexus + "snapshots/")
        else
          Some("Despegar Releases" at nexus + "releases/")
    },
    distHack <<= (target in SbtNativePackager.Universal, universal.Keys.name in SbtNativePackager.Universal) map { (d, n) => d / (n + ".zip")},
    ReleaseKeys.versionBump := Version.Bump.Bugfix,
    publish <<= publish dependsOn universal.Keys.dist,
    publishLocal <<= publishLocal dependsOn universal.Keys.dist,
    artifact in distHack <<= moduleName(n => Artifact(n, "zip", "zip")),
    publishMavenStyle in publish := true,
    publishMavenStyle in publishLocal := true,
    ReleaseKeys.releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      setReleaseVersion,
      //runClean, // if descomented npm fails
      runTest,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges)
  ) ++ Seq(addArtifact(artifact in distHack, distHack).settings: _*)

*/
}