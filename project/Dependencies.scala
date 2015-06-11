import sbt.Keys._
import sbt._

object Dependencies {

  val jooqVersion = "3.6.1"
  val scalaV = "2.11.6"

  lazy val macrosLibraryDependencies = Seq(
    "org.jooq"               %  "jooq"                 % jooqVersion,
    "org.jooq"               %  "jooq-meta"            % jooqVersion,
    "org.jooq"               %  "jooq-codegen"         % jooqVersion,
    "org.jooq"               %  "jooq-scala"           % jooqVersion,
    "org.scala-lang"         % "scala-reflect"         % scalaV,
    "org.scalatest"         %% "scalatest"             % "2.2.5" % Test,
    "com.h2database"         % "h2"                    % "1.4.187" % Test,
    "com.typesafe"           % "config"                % "1.2.1" % Test,
    "com.google.guava"       % "guava"                 % "18.0"
  )

  lazy val settings = Seq(
    scalaVersion := scalaV,
    libraryDependencies ++= macrosLibraryDependencies
  )
}