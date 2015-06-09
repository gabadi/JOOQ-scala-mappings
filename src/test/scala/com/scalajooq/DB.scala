package com.scalajooq

import org.h2.jdbcx.JdbcDataSource
import org.jooq.{Configuration, TransactionalRunnable, DSLContext}
import org.jooq.impl.{DSL, DataSourceConnectionProvider, DefaultConfiguration}
import org.jooq.tools.jdbc.JDBCUtils

import scala.util.control.NonFatal

/**
 * Created by gabadi on 6/9/15.
 */
object DB {

  private lazy val dsl = {
    val connectionString: String = "jdbc:h2:tcp://localhost:9092/~/target/testing;MODE=MYSQL"

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
