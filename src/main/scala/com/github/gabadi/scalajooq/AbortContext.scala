package com.github.gabadi.scalajooq

import com.github.gabadi.scalajooq.JooqMacroMapper._
import org.jooq.{Record, Table}

trait AbortContext[C <: Context] {

  /** The macro context (of type `C`), must be provided by the inheritor */
  protected val context: C
  protected val namespace: Option[String]

  import context.universe._

  def abortCanOnlyMapCaseClasses(symbol: context.universe.ClassSymbol)(macroContext: Option[String]) =
    abort(
      s"""
         |Mappings error:
         |$symbol isn't a case class, only case classes are supported""".stripMargin)(macroContext)

  def abortCanNotFindImplicitConversion(from: Type, to: Type)(macroContext: Option[String]) = {
    abort(
      s"""
         |Mappings error between:
         |can not find an implicit conversion from $from to $to""".stripMargin)(macroContext)
  }

  def abortCanNotFindMapIdFieldBetween(table: context.universe.Type, entityMethod: String)(macroContext: Option[String]) = {
    abort(
      s"""
         |Mappings error between:
         |can not map $entityMethod with table $table""".stripMargin)(macroContext)
  }

  def assertIsJooqMeta(mapper: context.universe.Tree)(macroContext: Option[String]) = {
    if (!(mapper.tpe <:< typeOf[JooqMeta[_, _, _]])) {
      abort(
        s"""
           |Mappings error between:
           |$mapper must be an instance of ${weakTypeOf[JooqMeta[_, _, _]]}""".stripMargin)(macroContext)
    }
  }

  def abortCanNotFindImplicitConversion(from: String, to: String)(macroContext: Option[String]) = {
    abort(
      s"""
         |Mappings error between:
         |Can not find an implicit conversion between $from and $to""".stripMargin)(macroContext)
  }

  def abortFieldCanNotBeMapped(field: String)(macroContext: Option[String]) = {
    abort(
      s"""
         |Mappings error:
         |Tried to create mappings for $field
          |Failed with:
          |""".stripMargin)(macroContext)
  }

  def abortFieldNotFoundInRecord(entity: String, record: String)(macroContext: Option[String]) = {
    abort(
      s"""
         |Mappings error:
         |$entity expects a $record column, but doesn't exists""".stripMargin)(macroContext)
  }

  def abort(msg: String)(child: Option[String] = None) = {
    child match {
      case None => throw new AbortException(msg)
      case Some(c) =>
        val sb = StringBuilder.newBuilder
        sb.append(msg)
        c.split(System.lineSeparator()).foreach(l => sb.append(s"##$l${System.lineSeparator()}"))
        throw new AbortException(sb.toString())
    }
  }

  def abortMissingMappingFor(entity: String, missingType: Type)(macroContext: Option[String]) = {
    val expectedJooqMeta = appliedType(typeOf[JooqMeta[_, _, _]].typeConstructor, typeOf[Table[_ <: Record]], typeOf[Record], missingType)
    abort(
      s"""
         |Mappings error:
         |can not map $entity expects a $expectedJooqMeta mapper, but does not exists""".stripMargin)(macroContext)
  }


}

object AbortContext {
  type Context = scala.reflect.macros.blackbox.Context
}
