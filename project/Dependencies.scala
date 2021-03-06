import sbt.Keys._
import sbt._

object Dependencies {

  lazy val macrosLibraryDependencies = Seq(
    "org.jooq" % "jooq" % jooqVersion % Provided,
    "org.jooq" % "jooq-meta" % jooqVersion % Provided,
    "org.jooq" % "jooq-scala" % jooqVersion % Test,
    "org.scala-lang"         % "scala-reflect"         % scalaV,
    "org.scalatest"         %% "scalatest"             % "2.2.5" % Test,
    "com.h2database"         % "h2"                    % "1.4.187" % Test,
    "com.typesafe"           % "config"                % "1.2.1" % Test,
    "com.twitter"           %% "util-eval"             % "6.24.0" % Test,
    "com.google.guava"       % "guava"                 % "18.0"
  )

  lazy val settings = Seq(
    scalaVersion := scalaV,
    libraryDependencies ++= macrosLibraryDependencies
  )
  val jooqVersion = "3.6.2"
  val scalaV = "2.11.7"
  val paradiseV = "2.1.0-M5"
  //val paradiseV = "2.0.1"
}