package com.github.gabadi.scalajooq

import com.github.gabadi.scalajooq.meta.JooqMetaType
import org.jooq.impl.TableImpl
import org.jooq.{DSLContext, Record, RecordMapper, Table}

import scala.language.existentials
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.{Failure, Success}

trait JooqMeta[T <: Table[R], R <: Record, E] extends RecordMapper[Record, E] {

  val table: T

  lazy val selectTable = joinedTable(table.asInstanceOf[Table[Record]])

  def joinedTable(current: Table[Record], namespace: Option[String] = None, leftJoin: Boolean = false): Table[Record]

  lazy val fields = selectTable.fields()

  override def map(record: Record): E = toEntity(record)

  def toEntity(record: Record): E

  def toOptEntity[G >: R <: Record](record: Record): Option[E] = {
    val isNull = fields.forall(f => null == record.getValue(f))
    if (isNull) None else Some(toEntity(record))
  }

  def toRecord(e: E, current: R = null.asInstanceOf[R])(implicit dsl: DSLContext): R

  def query(implicit dsl: DSLContext) = dsl.select(fields: _*).from(selectTable)

}

object JooqMeta {

  def metaOf[T <: TableImpl[R], R <: Record, E]: JooqMeta[T, R, E] =
  macro materializeRecordMapperImpl[T, R, E]

  def materializeType[T]: (T) => T = macro materializeTypeImpl[T]

  def namespacedMetaOf[T <: TableImpl[R], R <: Record, E](namespace: String): JooqMeta[T, R, E] =
  macro materializeNamespacedRecordMapperImpl[T, R, E]

  def materializeRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[JooqMeta[T, R, E]] = {
    createMetaImpl[T, R, E](c, None)
  }


  def materializeNamespacedRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(namespace: c.Expr[String]): c.Expr[JooqMeta[T, R, E]] = {
    createMetaImpl[T, R, E](c, Some(c.eval(namespace)))
  }

  def materializeTypeImpl[T: c.WeakTypeTag](c: Context): c.Expr[(T) => T] = {
    import c.universe._

    val tType = weakTypeOf[T]
    c.Expr[(T) => T](
      q"""
          {
            def function(t: $tType):$tType = t
            function
          }
        """)
  }

  def createMetaImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context, namespace: Option[String]): c.Expr[JooqMeta[T, R, E]] = {
    import c.universe._
    val jooqMacroMapper = {
      val n = namespace
      new {
        val co: c.type = c
        val namespace = n
      } with JooqMacroMapper[c.type]
    }

    val tableType = weakTypeOf[T]
    val entityType = weakTypeOf[E]
    val recordType = weakTypeOf[R]

    val body = jooqMacroMapper.tryGenerateByType(tableType = tableType, entityType = entityType, recordType = recordType)
    body match {
      case Success(b) =>
        val code = q"""
      new ${weakTypeOf[JooqMeta[T, R, E]]} {
        ..$b
      }
    """
        c.Expr[JooqMeta[T, R, E]] {
          code
        }

      case Failure(e: AbortException) =>
        c.abort(c.enclosingPosition, e.getMessage)
      case Failure(e) =>
        throw e
    }
  }

}

import scala.annotation.StaticAnnotation

object meta {
  type JooqMetaType = JooqMeta[T, R, E] forSome {type E; type R <: Record; type T <: Table[R]}
}

class meta(namespace: String = "") extends StaticAnnotation {
  def macroTransform(annottees: Any*): JooqMetaType = macro metaMacro.impl
}

object metaMacro {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[JooqMetaType] = {

    import c.universe._
    def expandMacro(parents: Any) = {
      val namespace = c.macroApplication match {
        case Apply(Select(Apply(_, List(Literal(Constant(s: String)))), _), _) =>
          if (s == null.asInstanceOf[String] || s.equals("")) None else Some(s)
        case _ => None
      }
      val jooqMacroMapper = {
        val n = namespace
        new {
          val co: c.type = c
          val namespace = n
        } with JooqMacroMapper[c.type]
      }

      val parentsSeq: Seq[AppliedTypeTree] = parents.asInstanceOf[Seq[AppliedTypeTree]]
      val rootTypes = parentsSeq.flatMap(p => p.children.headOption.map(t => t.toString().split("\\.").last)).toSet
      if (!rootTypes.contains("JooqMeta")) {
        c.abort(c.enclosingPosition, s"Needs to extend JooqMeta")
      }

      val tableType = parentsSeq.head.children(1)
      val recordType = parentsSeq.head.children(2)
      val entityType = parentsSeq.head.children(3)

      val macroBody = jooqMacroMapper.tryGenerateByTree(tableType = tableType, recordType = recordType, entityType = entityType)

      macroBody match {
        case Success(b) =>
          b
        case Failure(e: AbortException) =>
          c.abort(c.enclosingPosition, e.getMessage)
        case Failure(e) =>
          throw e
      }
    }

    val result = {
      annottees.map(_.tree).toList match {
        case q"$pref object $name extends ..$parents { ..$body }" :: Nil =>
          val code = q"""
              object $name extends ..$parents {
                ..$body
                ..${expandMacro(parents)}
              }"""
          code
      }
    }
    c.Expr[JooqMetaType](result)
  }
}

