package org.scalajooq

import com.google.common.base.CaseFormat.{LOWER_CAMEL, UPPER_UNDERSCORE}
import org.jooq.impl.TableImpl
import org.jooq.{DSLContext, Record, RecordMapper, Table}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}

trait JooqMeta[R <: Record, E] extends RecordMapper[Record, E] {

  val table: org.jooq.Table[R]

  val selectTable: org.jooq.Table[Record]

  lazy val fields = selectTable.fields()

  lazy val aliasedFields = fields.map(f => withAlias(f))

  def withAlias[T](field: org.jooq.Field[T]): org.jooq.Field[T] = field.as(field.toString)

  override def map(record: Record): E = toEntity(record)

  def toEntity(record: Record): E = toEntityAliased(record, false)

  def toEntityAliased(record: Record, aliased: Boolean = true): E

  def toOptEntity[T >: R <: Record](record: Record): Option[E] = {
    val isNull = fields.forall(f => null == record.getValue(f))
    if (isNull) None else Some(toEntity(record))
  }

  def toOptEntityAliased[T >: R <: Record](record: Record): Option[E] = {
    val isNull = aliasedFields.forall(f => null == record.getValue(f))
    if (isNull) None else Some(toEntityAliased(record))
  }

  def toRecord(record: E)(implicit dsl: DSLContext): R

  def query(implicit dsl: DSLContext) = dsl.select(selectTable.fields(): _*).from(selectTable)

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
    if (refs.size() > 1) {
      throw new IllegalStateException(s"Only supported one relation, but found $refs relations between $from and $to")
    }
    refs.get(0)
  }
}

object JooqMeta {
  implicit def metaOf[T <: TableImpl[R], R <: Record, E]: JooqMeta[R, E] =
  macro materializeRecordMapperImpl[T, R, E]


  def materializeRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[JooqMeta[R, E]] = {
    import c.universe._

    def implicitConversion(from: Type, to: Type) = c.inferImplicitValue(
      appliedType(typeOf[(_) => _].typeConstructor, from, to))

    def abortCanNotFindImplicitConversion(from: String, to: String) = {
      c.abort(c.enclosingPosition,
        s"""
           |Mappings error between:
           | Can not find an implicit conversion between $from and $to
           """.stripMargin)
    }


    def checkCaseClass(symbol: c.universe.ClassSymbol) = {
      if (!symbol.isClass || !symbol.isCaseClass)
        c.abort(c.enclosingPosition, "Can only map case classes.")
    }

    def caseClassFields(caseClass: c.universe.Type) = {
      checkCaseClass(caseClass.typeSymbol.asClass)
      val declarations = caseClass.decls
      val ctor = declarations.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.get
      ctor.paramLists.head
    }

    def tableMembersMap(tableType: c.universe.Type) = tableType.members.map(m => UPPER_UNDERSCORE.to(LOWER_CAMEL, m.name.decodedName.toString) -> m).toMap

    def recordMembersMap(recordType: c.universe.Type) = recordType.members
      .map(m => m.name.decodedName.toString -> m)
      .toMap



    val tableType = weakTypeOf[T]
    val entityType = weakTypeOf[E]
    val recordType = weakTypeOf[R]

    val fields = caseClassFields(entityType)

    val tableMembers = tableMembersMap(tableType)
    val recordMembers = recordMembersMap(recordType)

    val (toEntityParams, toRecordParams, maybeEmbedded) = fields.map { field =>
      val fieldName = field.name.decodedName.toString
      val fieldTermName = field.asTerm.name
      val columnName = LOWER_CAMEL.to(UPPER_UNDERSCORE, fieldName)
      val fieldIsOption = field.typeSignature <:< typeOf[Option[_]]
      val effectiveFieldType = if (fieldIsOption) field.typeSignature.typeArgs.head else field.typeSignature

      tableMembers.get(fieldName) match {
          // one to one entity - record matching
        case Some(recordMember) =>
          val recordSetter = recordMembers.get(s"set${fieldName.capitalize}").get

          val recordFieldType = recordSetter.asMethod.paramLists.head.head.typeSignature

          val e2rTypeConversion = implicitConversion(from = effectiveFieldType, to = recordFieldType)
          val r2eTypeConversion = implicitConversion(from = recordFieldType, to = effectiveFieldType)

          if ((r2eTypeConversion equalsStructure EmptyTree) || (e2rTypeConversion equalsStructure EmptyTree)) {
            abortCanNotFindImplicitConversion(s"$entityType.$fieldName($effectiveFieldType)", s"$recordType.$columnName($recordFieldType)")
          } else {
            if (fieldIsOption) {
              (q"$fieldTermName = Option(if(aliased) r.getValue(withAlias(table.$recordMember)) else r.getValue(table.$recordMember)).map($r2eTypeConversion)",
                q"r.$recordSetter((e.$fieldTermName).map($e2rTypeConversion).orNull[$recordFieldType])",
                None)
            } else {
              ( q"""$fieldTermName = $r2eTypeConversion(org.scalajooq.Constraints.checkNotNull(if(aliased) r.getValue(withAlias(table.$recordMember)) else r.getValue(table.$recordMember), ${recordMember.name.decodedName.toString} + " in record " + ${recordType.typeSymbol.name.decodedName.toString} + "  is null in the database. This is inconsistent"))""",
                q"""r.$recordSetter(org.scalajooq.Constraints.checkNotNull($e2rTypeConversion(e.$fieldTermName), $fieldName + " in entity " + ${entityType.typeSymbol.name.decodedName.toString} + " must not be null"))""",
                None)
            }
          }
        case None =>
          c.abort(c.enclosingPosition,
            s"""
               |Mappings error:
               | $entityType.$fieldName expects a $recordType.$columnName column, but doesn't exists
               | if this is a one to one relation a $recordType.${columnName}_ID column is expected
             """.stripMargin)
          /*
          val optRecordMethod = recordMembers.get(s"set${fieldName.capitalize}Oid").orElse(recordMembers.get(s"set${fieldName.capitalize}Id"))
          val expectedImplicitMapperType = appliedType(typeOf[RecordMapper[_, _]].typeConstructor, typeOf[Record], effectiveFieldType)
          val implicitMapper = c.inferImplicitValue(expectedImplicitMapperType) // puede fallar
          optRecordMethod match {
            case _ if implicitMapper.equalsStructure(EmptyTree) =>
              c.abort(c.enclosingPosition,
                s"""
                   |Mappings error:
                   | $entityType.$fieldName expects a $recordType.$columnName column, but doesn't exists
                                                                               |if this is a one to one relation a $recordType.${columnName}_ID column is expected
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
              if (fieldName.endsWith("Id") || fieldName.endsWith("Oid")) {
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
                                                                                 |if this is a one to one relation a $recordType.${columnName}_ID column is expected
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
                       |Invalid embedded entity ${field.typeSignature} for mapping in $entityType.$field, only is supported if it has an id field defined
             """.stripMargin)
              }
          }*/
      }
    }.unzip3


    val companion = entityType.typeSymbol.companion
    val embedded = maybeEmbedded.flatMap(o => o)
    /*
    val joins = embedded.map { embeddedMapper =>
      val table = embeddedMapper.tpe.decls.find(_.name.decodedName.toString().equals("table")).get
      val selectTable = embeddedMapper.tpe.decls.find(_.name.decodedName.toString().equals("selectTable")).get
      q"t = $embeddedMapper.$selectTable.join(t).onKey(org.scalajooq.Constraints.getUniqueReferenceTo(table.asInstanceOf[org.jooq.Table[org.jooq.Record]], $embeddedMapper.$table.asInstanceOf[org.jooq.Table[org.jooq.Record]]))"
    }*/

    val tableCompanion = tableType.typeSymbol.companion
    val companionMethod = TermName(LOWER_CAMEL.to(UPPER_UNDERSCORE, tableType.typeSymbol.name.decodedName.toString))

    val code = q"""
      new ${weakTypeOf[JooqMeta[R, E]]} {
        override val table = $tableCompanion.$companionMethod
        override val selectTable = {
           var t = table.asInstanceOf[${weakTypeOf[Table[Record]]}]
           t
        }
        override def toEntityAliased(r: ${weakTypeOf[Record]}, aliased: Boolean = true) = $companion(..$toEntityParams)
        override def toRecord(e: $entityType)(implicit dsl: org.jooq.DSLContext): $recordType = {
          val r = dsl.newRecord(table)
          ..$toRecordParams
          r
        }
      }
    """


    c.Expr[JooqMeta[R, E]] {
      code
    }
  }

}
