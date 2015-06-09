import java.net.Socket

import sbt._
import sbt.Keys._
import sbt.Tests.{InProcess, Group}

import scala.io.Source
import scala.util.control.NonFatal

object TestSettings {

  object DBTasks {
    val dbStart = TaskKey[Unit]("dbStart", "Starts the DB.")
    val dbStop = TaskKey[Unit]("dbStop", "Stop the DB.")

    val settings = Seq[Setting[_]](
      compile in Test <<= compile in Test dependsOn dbStart,
      dbStart <<= (managedClasspath in Test, streams).map((loader, s) => {
        val jar = loader.find(_.data.getName.startsWith("h2-")).get.data
        try {
          new Socket("localhost", 9092)
          Fork.java(ForkOptions(bootJars = jar :: Nil), "org.h2.tools.Server" :: "-tcpShutdown" :: "tcp://localhost:9092" :: Nil)
        } catch {
          case NonFatal(e) =>
        }
        Fork.java.fork(ForkOptions(bootJars = jar :: Nil), "org.h2.tools.Server" :: "-tcp" :: "-tcpPort" :: "9092" :: Nil)
      }),
      dbStop <<= (fullClasspath in Test).map(loader => {
        val jar = loader.find(_.data.getName.startsWith("h2-")).get.data
        Fork.java(ForkOptions(bootJars = jar :: Nil), "org.h2.tools.Server" :: "-tcpShutdown" :: "tcp://localhost:9092" :: Nil)
      })
    )
  }


  val testSettings = DBTasks.settings ++ Seq(
    testOptions in Test += Tests.Cleanup(loader => {
      loader.loadClass("org.h2.tools.Server").getMethod("main", classOf[Array[String]]).invoke(null, Array("-tcpShutdown", "tcp://localhost:9092"))
    })
  )
}

