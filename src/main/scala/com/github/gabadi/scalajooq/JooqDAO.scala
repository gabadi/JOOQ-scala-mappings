package com.github.gabadi.scalajooq

import org.jooq._
import org.jooq.impl.DSL._

import _root_.scala.collection.JavaConverters._
import _root_.scala.language.implicitConversions


abstract class JooqDAO[Rec <: UpdatableRecord[Rec], ID, Entity](implicit meta: JooqMeta[_ <: Table[Rec], Rec, Entity]) {

  require(meta.table.getPrimaryKey != null, s"The table ${meta.table} must have a primary key to be compatible with the JooqDAO")

  lazy val immutableFields = Set.empty[Field[_]]
  lazy val table = meta.table
  lazy val primaryKeys = List() ++ table.getPrimaryKey.getFields.asScala
  lazy val primaryKey = {
    if (
      primaryKeys.size == 1 &&
        (primaryKeys.head.getDataType.getType.equals(classOf[java.lang.Long])
          ||
          primaryKeys.head.getDataType.getType.equals(classOf[Integer]))
    )
      Some(primaryKeys.head.asInstanceOf[TableField[Rec, ID]])
    else None
  }
  lazy val createFields = (if (primaryKey.isDefined) meta.fields.filterNot(f => primaryKeys.contains(f)) else meta.fields).filter(f => meta.table.fields.exists(_.equals(f)))
  lazy val updateFields = createFields.filterNot(f => immutableFields.contains(f)).filterNot(f => primaryKeys.contains(f))

  def findAll(implicit dsl: DSLContext): List[Entity] = List() ++ meta.query(dsl).fetch(meta).asScala

  def insert(e: Entity, en: Entity*)(implicit dsl: DSLContext): Unit = insertAll(en :+ e)

  def insertAll(e: Seq[Entity])(implicit dsl: DSLContext): Unit = e.foreach(insert)

  def insert(e: Entity)(implicit dsl: DSLContext): ID = {
    val r = attached(e)(dsl)
    r.store(createFields: _*)
    if (primaryKey.isDefined) {
      r.getValue(primaryKey.get)
    } else {
      compositeKeyRecord(primaryKeys.map(f => r.getValue(f)))
    }
  }

  protected def compositeKeyRecord(values: List[Any])(implicit dsl: DSLContext): ID = {
    val key = table.getPrimaryKey
    if (key == null) {
      throw new RuntimeException(s"table: $table does not has a defined primary key")
    }
    val fields = key.getFieldsArray.asInstanceOf[Array[Field[_]]]
    val result: Record = dsl.newRecord(fields: _*)
    var i = 0
    while (i < values.length) {
      result.setValue(fields(i).asInstanceOf[Field[Any]], fields(i).getDataType.convert(values(i)))
      i = i + 1
    }
    result.asInstanceOf[ID]
  }

  implicit def detached(r: Rec): Entity = unsafeDetached(r)

  def unsafeDetached(r: Record): Entity = meta.toEntity(r)

  def update(e: Entity, en: Entity*)(implicit dsl: DSLContext): Int = updateAll(en :+ e)

  def updateAll(e: Seq[Entity])(implicit dsl: DSLContext): Int = e.map(update).sum

  def update(e: Entity)(implicit dsl: DSLContext): Int = e.update(updateFields: _*)

  def delete(e: Entity, en: Entity*)(implicit dsl: DSLContext): Int = deleteAll(en :+ e)

  def deleteAll(es: Seq[Entity])(implicit dsl: DSLContext): Int = es.map(delete).sum

  def delete(e: Entity)(implicit dsl: DSLContext): Int = attached(e).delete()

  implicit def attached(e: Entity)(implicit dsl: DSLContext): Rec = meta.toRecord(e)(dsl)

  def deleteAll()(implicit dsl: DSLContext): Int = (dsl delete table).execute()

  def deleteById(id: ID)(implicit dsl: DSLContext): Int = deleteByIds(id :: Nil)

  def deleteByIds(ids: Seq[ID])(implicit dsl: DSLContext): Int = {
    if (primaryKey.isDefined) {
      (dsl.
        delete(table)
        where (primaryKey.get in ids.asJava)
        execute())
    } else {
      ids.map { id =>
        dsl.delete(table).where(equal(primaryKeys, id)).execute()
      }.sum
    }
  }

  private def equal(pk: List[TableField[Rec, _]], id: ID): Condition = {
    if (pk.length == 1) {
      pk.head.asInstanceOf[Field[ID]].equal(pk.head.getDataType.convert(id).asInstanceOf[ID])
    } else {
      row(pk: _*).equal(id.asInstanceOf[Record])
    }
  }

  def deleteById(id: ID, idn: ID*)(implicit dsl: DSLContext): Int = deleteByIds(idn :+ id)

  def findById(id: ID)(implicit dsl: DSLContext): Option[Entity] = findByIds(id :: Nil).headOption

  def findByIds(ids: Seq[ID])(implicit dsl: DSLContext): List[Entity] = {
    List() ++ (
      if (primaryKey.isDefined) {
        (dsl.
          select(meta.fields: _*)
          from meta.selectTable
          where (primaryKey.get in ids.asJava)
          ).fetch(meta).asScala
      } else {
        ids.flatMap(id => dsl.selectFrom(table).where(equal(primaryKeys, id)).fetch(meta).asScala)
      }
      )
  }

  def findByIds(id: ID, idn: ID*)(implicit dsl: DSLContext): List[Entity] = findByIds(idn :+ id)
}


trait DefaultJooqDAO[Rec <: UpdatableRecord[Rec], Entity] extends JooqDAO[Rec, Long, Entity]

trait GenericJooqDAO[Rec <: UpdatableRecord[Rec], ID, Entity] extends JooqDAO[Rec, ID, Entity]
