package com.scalajooq

import com.typesafe.config.ConfigFactory
import org.h2.jdbcx.JdbcDataSource
import org.jooq.{Configuration, TransactionalRunnable, DSLContext}
import org.jooq.impl.{DSL, DataSourceConnectionProvider, DefaultConfiguration}
import org.jooq.tools.jdbc.JDBCUtils

import scala.util.control.NonFatal

object DB {


  private lazy val dsl = {
    val conf = ConfigFactory.load()

    val connectionString: String = conf.getString("db.url")

    val configuration = new DefaultConfiguration()

    val ds = new JdbcDataSource()
    ds.setUrl(connectionString)

    val dialect = JDBCUtils.dialect(connectionString)
    configuration.set(dialect)
    configuration.set(new DataSourceConnectionProvider(ds))
    DSL using configuration
  }

  private final class ProgrammaticRollbackException() extends Exception

  def withRollback(block: DSLContext => Unit) = {
    try {
      dsl.transaction(new TransactionalRunnable {
        override def run(configuration: Configuration): Unit = {
          block(DSL using configuration)
          throw new ProgrammaticRollbackException()
        }
      })
    } catch {
      case NonFatal(e) if e.getCause.isInstanceOf[ProgrammaticRollbackException] =>

    }
  }

}
