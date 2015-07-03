package com.github.gabadi.scalajooq

import com.twitter.util.Eval
import org.scalatest.{Inside, Matchers, WordSpec}

trait BaseSpec extends WordSpec with Inside with Matchers {

  def assertNoCompiles(code: String, message: String*) = {
    val e = the [Exception] thrownBy new Eval().check(code)
    e.getMessage should not include "ScalaReflectionException"
    message.foreach(m => e.getMessage should include(m))
  }



}
