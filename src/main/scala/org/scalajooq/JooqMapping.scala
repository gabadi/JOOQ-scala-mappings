package com.scalajooq

import com.google.common.base.CaseFormat.{LOWER_CAMEL, UPPER_UNDERSCORE}
import org.jooq.impl.TableImpl
import org.jooq.{DSLContext, Record, RecordMapper, Table}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}

trait JooqMapping[R <: Record, E] extends RecordMapper[Record, E] {
  override def map(record: Record): E = ???
  /*
  val table: org.jooq.Table[R]

  val selectTable: org.jooq.Table[Record]

  lazy val fields = selectTable.fields()

  lazy val aliasedFields = fields.map(f => withAlias(f))

  def withAlias[T](field: org.jooq.Field[T]):org.jooq.Field[T] = field.as(field.toString)

  override def map(record: Record): E = toEntity(record)

  def toEntity(record: Record): E = toEntityAliased(record, false)

  def toEntityAliased(record: Record, aliased: Boolean = true): E

  def toOptEntity[T >: R <: Record](record: Record, tabl: Table[T] = selectTable): Option[E] = {
    val isNull = fields.forall(f => null == record.getValue(f))
    if(isNull) None else Some(toEntity(record))
  }

  def toOptEntityAliased[T >: R <: Record](record: Record, tabl: Table[T] = selectTable): Option[E] = {
    val isNull = aliasedFields.forall(f => null == record.getValue(f))
    if(isNull) None else Some(toEntityAliased(record))
  }

  def toRecord(record: E)(implicit dsl: DSLContext): R

  def query(implicit dsl: DSLContext) = dsl.select(selectTable.fields() :_*).from(selectTable)
*/
}

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
    if(refs.size() > 1) {
      throw new IllegalStateException(s"Only supported one relation, but found $refs relations between $from and $to")
    }
    refs.get(0)
  }
}

object JooqMacros {
  implicit def recordMapper[T <: TableImpl[R], R <: Record, E]: JooqMapping[R, E] =
  macro materializeRecordMapperImpl[T, R, E]

  def materializeRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[JooqMapping[R, E]] = {
    import c.universe._

    def implicitConversion(from: Type, to: Type) = c.inferImplicitValue(
      appliedType(typeOf[(_) => _].typeConstructor, from, to))


    val tableType = weakTypeOf[T]
    val entityType = weakTypeOf[E]
    val recordType = weakTypeOf[R]
    val entitySymbol = entityType.typeSymbol.asClass

    if (!entitySymbol.isClass || !entitySymbol.isCaseClass)
      c.abort(c.enclosingPosition, "Can only map case classes.")


    val tableMembers = tableType.members.map(m => UPPER_UNDERSCORE.to(LOWER_CAMEL, m.name.decodedName.toString) -> m).toMap
    val recordMembers = recordType.members
      .map(m => m.name.decodedName.toString -> m)
      .toMap

    val declarations = entityType.decls
    val ctor = declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get

    val fields = ctor.paramLists.head
    val (toEntityParams, toRecordParams, maybeEmbedded) = fields.map { field =>
      val fieldName = field.name.decodedName.toString
      val columnName = LOWER_CAMEL.to(UPPER_UNDERSCORE, fieldName)
      tableMembers.get(fieldName) match {
        case Some(recordMember) =>
          val mapKey = field.asTerm.name
          val setter = recordMembers.get(s"set${fieldName.capitalize}").get

          val scalaFieldType = field.typeSignature
          val recordFieldType = setter.asMethod.paramLists.head.head.typeSignature

          field.typeSignature.find(scalaFieldSubType => {
            val scalaToRecord = implicitConversion(from = scalaFieldSubType, to = recordFieldType)
            val recordToScala = implicitConversion(from = recordFieldType, to = scalaFieldSubType)
            !(scalaToRecord equalsStructure EmptyTree) && !(recordToScala equalsStructure EmptyTree)
          }) match {
            case Some(scalaFieldSubType) =>
              val scalaToRecord = implicitConversion(from = scalaFieldSubType, to = recordFieldType)
              val recordToScala = implicitConversion(from = recordFieldType, to = scalaFieldSubType)

              if (scalaFieldType <:< typeOf[Option[_]]) {
                (q"$mapKey = Option(if(aliased) r.getValue(withAlias(table.$recordMember)) else r.getValue(table.$recordMember)).map($recordToScala)",
                  q"r.$setter((e.$mapKey).map($scalaToRecord).orNull[$recordFieldType])",
                  None)
              } else {
                ( q"""$mapKey = $recordToScala(com.despegar.sem.macros.Constraints.checkNotNull(if(aliased) r.getValue(withAlias(table.$recordMember)) else r.getValue(table.$recordMember), ${recordMember.name.decodedName.toString} + " in record " + ${recordType.typeSymbol.name.decodedName.toString} + "  is null in the database. There is inconsistent"))""",
                  q"""r.$setter(com.despegar.sem.macros.Constraints.checkNotNull($scalaToRecord(e.$mapKey), $fieldName + " in entity " + ${entityType.typeSymbol.name.decodedName.toString} + " must not be null"))""",
                  None)
              }
            case None =>
              c.abort(c.enclosingPosition,
                s"""
                   |Mappings error between:
                   |  Can not find an implicit conversion between $entityType.$fieldName($scalaFieldType) and $recordType.$columnName($recordFieldType)
                 """.stripMargin)
          }
        case None => {
          def effectiveFieldType = {
            if (field.typeSignature <:< typeOf[Option[_]]) {
              field.typeSignature.typeArgs.head
            } else {
              field.typeSignature
            }
          }
          val optRecordMethod = recordMembers.get(s"set${fieldName.capitalize}Oid").orElse(recordMembers.get(s"set${fieldName.capitalize}Id")) // puede fallar
          val expectedImplicitMapperType = appliedType(typeOf[RecordMapper[_, _]].typeConstructor, typeOf[Record], effectiveFieldType)
          val implicitMapper = c.inferImplicitValue(expectedImplicitMapperType) // puede fallar
          optRecordMethod match {
            case _ if implicitMapper.equalsStructure(EmptyTree) =>
              c.abort(c.enclosingPosition,
                s"""
                   |Mappings error:
                   | $entityType.$fieldName expects a $recordType.$columnName column, but doesn't exists
                                                                               | if this is a one to one relation a $recordType.${columnName}_ID column is expected
             """.stripMargin)
            case Some(_) if implicitMapper.symbol.isMacro =>
              c.abort(c.enclosingPosition,
                s"""
                   |Mappings error:
                   | $entityType.$fieldName expects an implicit $expectedImplicitMapperType, but doesn't exists
             """.stripMargin)
            case None if !implicitMapper.symbol.isMacro =>
              c.abort(c.enclosingPosition,
                s"""
                   |Mappings error:
                   | $entityType.$fieldName expects a $recordType.${columnName}_ID column, but doesn't exists
             """.stripMargin)
            case None =>
              if(fieldName.endsWith("Id") || fieldName.endsWith("Oid")) {
                c.abort(c.enclosingPosition,
                  s"""
                     |Mappings error:
                     | $entityType.$fieldName expects a $recordType.$columnName column, but doesn't exists
             """.stripMargin)
              } else {
                c.abort(c.enclosingPosition,
                  s"""
                     |Mappings error:
                     | $entityType.$fieldName expects a $recordType.$columnName column, but doesn't exists
                                                                                 | if this is a one to one relation a $recordType.${columnName}_ID column is expected
             """.stripMargin)
              }
            case Some(recordMethod) =>
              val converter = {
                if (field.typeSignature <:< typeOf[Option[_]]) {
                  implicitMapper.tpe.decls.find(_.name.decodedName.toString().equals("toOptEntityAliased")).get
                } else {
                  implicitMapper.tpe.decls.find(_.name.decodedName.toString().equals("toEntityAliased")).get
                }
              }
              val fieldMethod = entityType.members.find(_.name.decodedName.toString.equals(fieldName)).get

              effectiveFieldType.members.find(m => m.name.decodedName.toString.equals("oid") || m.name.decodedName.toString.equals("id")) match {
                case Some(embeddedMethod) =>
                  (q"$field = $implicitMapper.$converter(r, aliased)",
                    if (field.typeSignature <:< typeOf[Option[_]])
                      q"r.$recordMethod(e.$fieldMethod.map(i => i.$embeddedMethod).orNull)"
                    else
                      q"r.$recordMethod(e.$fieldMethod.$embeddedMethod)",
                    Some(implicitMapper))
                case None =>
                  c.abort(c.enclosingPosition,
                    s"""
                       |Mappings error:
                       | Invalid embedded entity ${field.typeSignature} for mapping in $entityType.$field, only is supported if it has an id field defined
             """.stripMargin)
              }
          }
        }
      }
    }.unzip3


    val companion = entityType.typeSymbol.companion
    val embedded = maybeEmbedded.flatMap(o => o)
    val joins = embedded.map{ embeddedMapper =>
      val table = embeddedMapper.tpe.decls.find(_.name.decodedName.toString().equals("table")).get
      val selectTable = embeddedMapper.tpe.decls.find(_.name.decodedName.toString().equals("selectTable")).get
      q"t = $embeddedMapper.$selectTable.join(t).onKey(com.despegar.sem.macros.Constraints.getUniqueReferenceTo(table.asInstanceOf[org.jooq.Table[org.jooq.Record]], $embeddedMapper.$table.asInstanceOf[org.jooq.Table[org.jooq.Record]]))"
    }

    val tableCompanion = tableType.typeSymbol.companion
    val companionMethod = TermName(LOWER_CAMEL.to(UPPER_UNDERSCORE, tableType.typeSymbol.name.decodedName.toString))


    c.Expr[JooqMapping[R, E]] { q"""
      new com.scalajooq.JooqMapping[$recordType, $entityType] {
      }
    """
    }
  }

}
