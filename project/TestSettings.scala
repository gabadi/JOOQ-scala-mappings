import java.io.{ByteArrayInputStream, File}
import java.net.Socket

import com.typesafe.config.ConfigFactory
import odelay.Timer
import org.apache.ivy.util.FileUtil
import org.jooq.util.GenerationTool
import sbt.Keys._
import sbt._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.matching.Regex.Match

// bring default timer into scope

import odelay.Timer.default

import scala.util.control.NonFatal

object TestSettings {

  object DBTasks {
    val dbStart = TaskKey[Seq[File]]("dbStart", "Starts the DB.")
    val dbStop = TaskKey[Unit]("dbStop", "Stop the DB.")

    val settings = Seq[Setting[_]](
      compile in Test <<= compile in Test dependsOn dbStart,
      managedSourceDirectories in Test += crossTarget.value / "jooq" / "test",
      sourceGenerators in Test <+= (dbStart in Test),
      dbStart <<= (managedClasspath in Test, streams, resourceDirectory in Test, crossTarget in Test).map((loader, s, resourceDirectory, ct) => {
        val conf = ConfigFactory.parseFileAnySyntax(new java.io.File(resourceDirectory, "application.conf"))

        val dbPort = conf.getString("db.port")
        val dbHost = conf.getString("db.host")
        val dbUrl = conf.getString("db.url")

        val h2jars = loader.filter(_.data.getName.startsWith("h2-")).map(_.data)
        val options = ForkOptions(bootJars = h2jars)
        val shoutDownCommand = "org.h2.tools.Server" :: "-tcpShutdown" :: s"tcp://$dbHost:$dbPort" :: Nil
        val startUpCommand = "org.h2.tools.Server" :: "-tcp" :: "-tcpPort" :: dbPort :: Nil

        def filtered(line: String) = {
          val pattern = """((?:\\?)\$\{.+?\})""".r

          def replacer() = (m: Match) => {
            m.matched match {
              case str if str.startsWith("\\") => Some( """\$\{%s\}""" format str.substring(3, str.length - 1))
              case str =>
                val key = str.substring(2, str.length - 1)
                if (!conf.hasPath(key))
                  None
                else {
                  Some(conf.getString(key))
                }
            }
          }
          pattern.replaceSomeIn(line, replacer())
        }


        def stopIfRunning = {
          try {
            new Socket("localhost", 9092)
            Fork.java(options, shoutDownCommand)
          } catch {
            case NonFatal(e) =>
          }
        }

        def runDbInFork = Fork.java.fork(options, startUpCommand)

        def waitTillDbStarted = {
          import scala.concurrent.ExecutionContext.Implicits.global

          implicit val success = retry.Success[Socket]((s) => true)

          Await.result(retry.Backoff()(implicitly[Timer])(() => scala.concurrent.Future {
            new Socket("localhost", 9092)
          }), 1 minute)
        }

        def recursiveListFiles(f: File): Seq[File] = {
          val files = Option(f.listFiles).toSeq.flatten
          files ++ files.filter(_.isDirectory).flatMap(recursiveListFiles)
        }

        def migrationCommand(scripts: Seq[File]) = {
          val migrations = scripts.map(s => s"RUNSCRIPT FROM '$s'").mkString("\\;")
          "org.h2.tools.Shell" :: "-url" :: s"$dbUrl;INIT=$migrations" :: "-sql" :: "show tables" :: Nil
        }

        def migrations = {
          val scripts = recursiveListFiles(resourceDirectory).filter(_.getName.toLowerCase.endsWith("test.sql"))
          Fork.java(options, migrationCommand(scripts = scripts))
        }

        def runJooqCodeGen() = {
          def filteredJooqConf(configFile: File) = {
            val result = StringBuilder.newBuilder
            for (line <- Source.fromFile(configFile).getLines()) {
              result append filtered(line)
              result append "\n"
            }
            GenerationTool.load(new ByteArrayInputStream(result.toString().getBytes))
          }
          val jooqConf = filteredJooqConf(new File(resourceDirectory, "jooq.xml"))

          jooqConf.getGenerator.getTarget.setDirectory((ct / "jooq" / "test").toString)
          FileUtil.forceDelete(ct / "jooq" / "test")
          GenerationTool.generate(jooqConf)
          recursiveListFiles(ct / "jooq" / "test").filter(_.isFile)
        }

        stopIfRunning
        runDbInFork
        waitTillDbStarted
        migrations
        runJooqCodeGen()
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

