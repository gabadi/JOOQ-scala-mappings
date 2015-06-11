logLevel := Level.Warn

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.4")

resolvers += "softprops-maven" at "http://dl.bintray.com/content/softprops/maven"

val jooqVersion = "3.6.1"

libraryDependencies ++= Seq(
  "me.lessis" %% "retry" % "0.2.0",
  "com.typesafe" % "config" % "1.2.1",
  "org.jooq" % "jooq" % jooqVersion,
  "org.jooq" % "jooq-meta" % jooqVersion,
  "org.jooq" % "jooq-codegen" % jooqVersion,
  "org.jooq" % "jooq-scala" % jooqVersion,
  "com.h2database" % "h2" % "1.4.187"
)