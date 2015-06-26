package com.github.gabadi.scalajooq

import com.google.common.base.CaseFormat
import com.google.common.base.CaseFormat.{UPPER_CAMEL, LOWER_CAMEL, UPPER_UNDERSCORE}
import org.jooq.impl.TableImpl
import org.jooq.{DSLContext, Record, RecordMapper, Table, Field}

import scala.language.experimental.macros
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}

trait JooqMeta[R <: Record, E] extends RecordMapper[Record, E] {

  val table: org.jooq.Table[R]

  val selectTable: org.jooq.Table[Record]

  def fields = selectTable.fields()

  val aliasedFields = fields.map(f => withAlias(f))

  def withAlias[T](field: org.jooq.Field[T]): org.jooq.Field[T] = field.as(field.toString)

  override def map(record: Record): E = toEntity(record)

  def toEntity(record: Record): E = toEntityAliased(record, false)

  def toEntityAliased(record: Record, aliased: Boolean = true): E

  def toOptEntity[T >: R <: Record](record: Record): Option[E] = toOptEntityAliased(record, false)

  def toOptEntityAliased[T >: R <: Record](record: Record, aliased: Boolean = true): Option[E] = {
    val isNull = (if (aliased) aliasedFields else fields).forall(f => null == record.getValue(f))
    if (isNull) None else Some(toEntityAliased(record, aliased))
  }

  def toRecord(e: E, current: R = null.asInstanceOf[R])(implicit dsl: DSLContext): R

  def query(implicit dsl: DSLContext) = dsl.select(fields: _*).from(selectTable)

}

object JooqMeta {

  def metaOf[T <: TableImpl[R], R <: Record, E]: JooqMeta[R, E] =
  macro materializeRecordMapperImpl[T, R, E]


  def namespacedMetaOf[T <: TableImpl[R], R <: Record, E](namespace: String): JooqMeta[R, E] =
  macro materializeNamespacedRecordMapperImpl[T, R, E]

  def materializeRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[JooqMeta[R, E]] = {
    createMetaImpl[T, R, E](c, "")
  }


  def materializeNamespacedRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(namespace: c.Expr[String]): c.Expr[JooqMeta[R, E]] = {
    createMetaImpl[T, R, E](c, c.eval(namespace))
  }


  def createMetaImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context, namespace: String): c.Expr[JooqMeta[R, E]] = {
    import c.universe._

    def implicitConversion(from: Type, to: Type) = c.inferImplicitValue(
      appliedType(typeOf[(_) => _].typeConstructor, from, to))

    def abortCanNotFindImplicitConversion(from: String, to: String) = {
      c.abort(c.enclosingPosition,
        s"""
           |Mappings error between:
           |Can not find an implicit conversion between $from and $to
           """.stripMargin)
    }

    def abortFieldCanNotBeMapped(field: String, tree: c.universe.Tree, message: String) = {
      c.abort(c.enclosingPosition,
        s"""
           |Mappings error:
           |Can not create the mapping for $field
           | Tried to: $tree
           | But failed with: $message
           """.stripMargin)
    }

    def abortFieldNotFoundInRecord(entity: String, record: String) = {
      c.abort(c.enclosingPosition,
        s"""
           |Mappings error:
           | $entity expects a $record column, but doesn't exists
                                        |if this is a one to one relation a ${record}_ID column is expected
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

    def tableMemberToName(symbol: c.universe.Symbol) = {
      val name = UPPER_UNDERSCORE.to(LOWER_CAMEL, symbol.name.decodedName.toString)
      if (name.startsWith(namespace)) UPPER_CAMEL.to(LOWER_CAMEL, name.drop(namespace.length)) else name
    }

    def tableMembersMap(tableType: c.universe.Type) = tableType.members.map(m => tableMemberToName(m) -> m).toMap

    def recordMembersMap(recordType: c.universe.Type) = recordType.members
      .map(m => m.name.decodedName.toString -> m)
      .toMap

    def tableInstanceMethod(tableType: c.universe.Type) = {
      val tableCompanion = tableType.typeSymbol.companion
      val companionMethod = TermName(LOWER_CAMEL.to(UPPER_UNDERSCORE, tableType.typeSymbol.name.decodedName.toString))
      q"$tableCompanion.$companionMethod"
    }

    val packag = q"com.github.gabadi.scalajooq"
    val jooqMeta = q"$packag.JooqMeta"
    val checkNotNull = q"$packag.Constraints.checkNotNull"


    val tableType = weakTypeOf[T]
    val entityType = weakTypeOf[E]
    val recordType = weakTypeOf[R]

    val fields = caseClassFields(entityType)
    val table = tableInstanceMethod(tableType) //q"$tableCompanion.$companionMethod"

    val tableMembers = tableMembersMap(tableType)
    val recordMembers = recordMembersMap(recordType)

    val (toEntityParams, toRecordParams, mappedFields) = fields.map { field =>
      val fieldName = field.name.decodedName.toString
      val fieldTermName = field.asTerm.name
      val columnName = LOWER_CAMEL.to(UPPER_UNDERSCORE, fieldName)
      val fieldIsOption = field.typeSignature <:< typeOf[Option[_]]
      val effectiveFieldType = if (fieldIsOption) field.typeSignature.typeArgs.head else field.typeSignature
      tableMembers.get(fieldName) match {
        // one to one entity - record matching
        case Some(recordMember) =>
          val recordSetter = recordMembers.get(s"set${namespace.capitalize}${fieldName.capitalize}").get

          val recordFieldType = recordSetter.asMethod.paramLists.head.head.typeSignature

          val e2rTypeConversion = implicitConversion(from = effectiveFieldType, to = recordFieldType)
          val r2eTypeConversion = implicitConversion(from = recordFieldType, to = effectiveFieldType)

          val getMaybeAliasedValue = q"if(aliased) r.getValue(withAlias(table.$recordMember)) else r.getValue(table.$recordMember)"

          if ((r2eTypeConversion equalsStructure EmptyTree) || (e2rTypeConversion equalsStructure EmptyTree)) {
            abortCanNotFindImplicitConversion(s"$entityType.$fieldName($effectiveFieldType)", s"$recordType.$columnName($recordFieldType)")
          } else {
            if (fieldIsOption) {
              (q"$fieldTermName = Option($getMaybeAliasedValue).map($r2eTypeConversion)",
                q"r.$recordSetter(e.$fieldTermName.map($e2rTypeConversion).orNull[$recordFieldType])",
                q"f = f ++ Array($table.$recordMember)")
            } else {
              val nullInRecordMessage = q"""${recordMember.name.decodedName.toString} + " in record " + ${recordType.typeSymbol.name.decodedName.toString} + "  is null in the database. This is inconsistent""""
              val nullInEntityMessage = q"""$fieldName + " in entity " + ${entityType.typeSymbol.name.decodedName.toString} + " must not be null""""
              (q"$fieldTermName = $r2eTypeConversion($checkNotNull($getMaybeAliasedValue, $nullInRecordMessage))",
                q"r.$recordSetter($checkNotNull($e2rTypeConversion(e.$fieldTermName), $nullInEntityMessage))",
                q"f = f ++ Array($table.$recordMember)")
            }
          }
        case None =>
          val implicitMapper = {
            val expectedImplicitMapperType = appliedType(typeOf[RecordMapper[_, _]].typeConstructor, typeOf[Record], effectiveFieldType)
            val implicitMapper = c.inferImplicitValue(expectedImplicitMapperType)
            if (!implicitMapper.equalsStructure(EmptyTree)) {
              implicitMapper
            } else {
              val newNamespace = s"$namespace${if (namespace.isEmpty) fieldName else fieldName.capitalize}"
              val newNamespaceUpper = LOWER_CAMEL.to(UPPER_UNDERSCORE, newNamespace)
              val mayExistNamespace = tableType.members.exists(_.name.decodedName.toString.startsWith(newNamespaceUpper))

              if (!mayExistNamespace) {
                abortFieldNotFoundInRecord(s"$entityType.$fieldName", s"$recordType.$columnName")
              } else {
                val tree = q"""$jooqMeta.namespacedMetaOf[$tableType, $recordType, $effectiveFieldType]($newNamespace)"""
                try {
                  c.typecheck(tree = tree).tpe
                } catch {
                  case e: TypecheckException =>
                    abortFieldCanNotBeMapped(s"$entityType.$fieldName", tree, e.getMessage)
                }
                tree
              }
            }
          }


          val toEntity = if (fieldIsOption) q"$implicitMapper.${TermName("toOptEntityAliased")}" else q"$implicitMapper.${TermName("toEntityAliased")}"
          val toRecord = q"$implicitMapper.${TermName("toRecord")}"

          (q"$field = $toEntity(r, aliased)",
            if (fieldIsOption) {
              q"e.${TermName(fieldName)}.foreach(o => $toRecord(o, r))"
            } else {
              q"$toRecord(e.${TermName(fieldName)}, r)"
            },
            q"f = f ++ $implicitMapper.${TermName("fields")}")
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
    /*
    val embedded = maybeEmbedded.flatMap(o => o)
    val joins = embedded.map { embeddedMapper =>
      val table = embeddedMapper.tpe.decls.find(_.name.decodedName.toString().equals("table")).get
      val selectTable = embeddedMapper.tpe.decls.find(_.name.decodedName.toString().equals("selectTable")).get
      q"t = $embeddedMapper.$selectTable.join(t).onKey(com.github.gabadi.scalajooq.Constraints.getUniqueReferenceTo(table.asInstanceOf[org.jooq.Table[org.jooq.Record]], $embeddedMapper.$table.asInstanceOf[org.jooq.Table[org.jooq.Record]]))"
    }*/

    val code = q"""
      new ${weakTypeOf[JooqMeta[R, E]]} {
        override val table = $table
        override val selectTable = {
           var t = table.asInstanceOf[${weakTypeOf[Table[Record]]}]
           t
        }
        override def fields = {
          var f = Array.empty[${weakTypeOf[Field[_]]}]
          ..$mappedFields
          f
        }
        override def toEntityAliased(r: ${weakTypeOf[Record]}, aliased: Boolean = true) = $companion(..$toEntityParams)
        override def toRecord(e: $entityType, current: $recordType = null.asInstanceOf[$recordType])(implicit dsl: org.jooq.DSLContext): $recordType = {
          val r = if(current != null) current else dsl.newRecord(table)
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
