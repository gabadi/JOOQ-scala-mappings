package com.github.gabadi.scalajooq

import org.jooq.Record

object Constraints {
  def checkNotNull[U](v: U) = {
    if (v == null) {
      throw new NullPointerException
    }
    v
  }

  def checkNotNull[U](v: U, error: => String) = {
    if (v == null) {
      throw new NullPointerException(error)
    }
    v
  }

  def getUniqueReferenceTo(from: org.jooq.Table[Record], to: org.jooq.Table[Record]) = {
    val refs = from.getReferencesTo(to)
    if (refs.size() > 1) {
      throw new IllegalStateException(s"Only supported one relation, but found $refs relations between $from and $to")
    }
    refs.get(0)
  }
}

