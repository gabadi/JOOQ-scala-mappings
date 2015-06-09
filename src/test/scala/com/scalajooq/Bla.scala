package com.scalajooq

import javax.sql.DataSource

import org.h2.jdbcx.JdbcDataSource
import org.jooq.DSLContext
import org.jooq.impl.{DSL, DataSourceConnectionProvider, DefaultConfiguration}
import org.jooq.tools.jdbc.JDBCUtils
import org.scalatest.{Matchers, WordSpec}

class Bla extends WordSpec with Matchers {

   "bla" in DB.withRollback{ dsl =>
     println(dsl.fetch("show tables"))
   }

 }
