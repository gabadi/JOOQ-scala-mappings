package com.scalajooq

import javax.sql.DataSource

import org.h2.jdbcx.JdbcDataSource
import org.jooq.DSLContext
import org.jooq.impl.{DSL, DataSourceConnectionProvider, DefaultConfiguration}
import org.jooq.tools.jdbc.JDBCUtils
import org.scalatest.{Matchers, WordSpec}

class BasicMapping extends WordSpec with Matchers {

   "bla" in DB.withRollback{ dsl =>
     //db.test.public.Public.PUBLIC
     println(dsl.fetch("show tables"))
   }

 }
