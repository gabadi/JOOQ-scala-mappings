package com.scalajooq

import org.scalatest.{Matchers, WordSpec}

class BasicMappingTest extends WordSpec with Matchers {

   "bla" in DB.withRollback{ dsl =>
     println(dsl.fetch("show tables"))
   }

 }
