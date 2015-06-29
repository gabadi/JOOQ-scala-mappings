package com.github.gabadi.scalajooq

import com.google.common.base.CaseFormat.{LOWER_CAMEL, UPPER_CAMEL, UPPER_UNDERSCORE}
import org.jooq.impl.TableImpl
import org.jooq.{DSLContext, Field, Record, RecordMapper, Table}

import scala.language.experimental.macros
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

trait JooqMeta[T <: Table[R], R <: Record, E] extends RecordMapper[Record, E] {

  val table: T

  lazy val selectTable = joinedTable(table.asInstanceOf[Table[Record]])

  def joinedTable(current: Table[Record], leftJoin: Boolean = false): Table[Record]

  lazy val fields = selectTable.fields()

  lazy val aliasedFields = fields.map(f => withAlias(f))

  def withAlias[G](field: org.jooq.Field[G]): org.jooq.Field[G] = field.as(field.toString)

  override def map(record: Record): E = toEntity(record)

  def toEntity(record: Record): E = toEntityAliased(record, false)

  def toEntityAliased(record: Record, aliased: Boolean = true): E

  def toOptEntity[G >: R <: Record](record: Record): Option[E] = toOptEntityAliased(record, false)

  def toOptEntityAliased[G >: R <: Record](record: Record, aliased: Boolean = true): Option[E] = {
    val isNull = (if (aliased) aliasedFields else fields).forall(f => null == record.getValue(f))
    if (isNull) None else Some(toEntityAliased(record, aliased))
  }

  def toRecord(e: E, current: R = null.asInstanceOf[R])(implicit dsl: DSLContext): R

  def query(implicit dsl: DSLContext) = dsl.select(fields: _*).from(selectTable)

}

object JooqMeta {

  def metaOf[T <: TableImpl[R], R <: Record, E]: JooqMeta[T, R, E] =
  macro materializeRecordMapperImpl[T, R, E]


  def namespacedMetaOf[T <: TableImpl[R], R <: Record, E](namespace: String): JooqMeta[T, R, E] =
  macro materializeNamespacedRecordMapperImpl[T, R, E]

  def materializeRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context): c.Expr[JooqMeta[T, R, E]] = {
    createMetaImpl[T, R, E](c, "")
  }


  def materializeNamespacedRecordMapperImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(namespace: c.Expr[String]): c.Expr[JooqMeta[T, R, E]] = {
    createMetaImpl[T, R, E](c, c.eval(namespace))
  }


  def createMetaImpl[T <: TableImpl[R] : c.WeakTypeTag, R <: Record : c.WeakTypeTag, E: c.WeakTypeTag](c: Context, namespace: String): c.Expr[JooqMeta[T, R, E]] = {
    import c.universe._

    def canNotFindMapIdFieldBetween(table: c.universe.Type, entityMethod: String) = {
      c.abort(c.enclosingPosition,
        s"""
           |Mappings error between:
           |can not map $entityMethod with table $table
           """.stripMargin)
    }

    def implicitConversion(from: Type, to: Type) = {
      val r = c.inferImplicitValue(
        appliedType(typeOf[(_) => _].typeConstructor, from, to))
      if (r.equalsStructure(EmptyTree)) {
        c.abort(c.enclosingPosition,
          s"""
             |Mappings error between:
             |can not find an implicit conversion from $from to $to
           """.stripMargin)
      }
      r
    }

    def assertIsJooqMeta(mapper: c.universe.Tree) = {
      if (!mapper.tpe.typeSymbol.equals(symbolOf[JooqMeta[_, _, _]])) {
        c.abort(c.enclosingPosition,
          s"""
             |Mappings error between:
             |$mapper must be an instance of ${weakTypeOf[JooqMeta[_, _, _]]}
           """.stripMargin)
      }
    }

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
            |Tried to: $tree
            |But failed with: $message
           """.stripMargin)
    }

    def abortFieldNotFoundInRecord(entity: String, record: String) = {
      c.abort(c.enclosingPosition,
        s"""
           |Mappings error:
           |$entity expects a $record column, but doesn't exists
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
    def checkNotNullTree(tree: Tree, message: String) = q"$packag.Constraints.checkNotNull($tree, $message)"


    val tableType = weakTypeOf[T]
    val entityType = weakTypeOf[E]
    val recordType = weakTypeOf[R]

    val fields = caseClassFields(entityType)
    val table = tableInstanceMethod(tableType) //q"$tableCompanion.$companionMethod"

    val tableMembers = tableMembersMap(tableType)
    val recordMembers = recordMembersMap(recordType)

    val (toEntityParams, toRecordParams, mappedFields, joins) = fields.map { field =>
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
                q"f = f ++ Array($table.$recordMember)",
                Nil)
            } else {
              val nullInRecordMessage = s"${recordMember.name.decodedName.toString} in record ${recordType.typeSymbol.name.decodedName.toString} is null in the database. This is inconsistent"
              val nullInEntityMessage = s"$fieldName in entity ${entityType.typeSymbol.name.decodedName.toString} must not be null"
              val entityFieldConverted = q"$e2rTypeConversion(e.$fieldTermName)"
              (q"$fieldTermName = $r2eTypeConversion(${checkNotNullTree(getMaybeAliasedValue, nullInRecordMessage)})",
                q"r.$recordSetter(${checkNotNullTree(entityFieldConverted, nullInEntityMessage)})",
                q"f = f ++ Array($table.$recordMember)",
                Nil)
            }
          }
        case None =>
          val implicitMapper = {
            val expectedImplicitMapperType = appliedType(typeOf[RecordMapper[_, _]].typeConstructor, typeOf[Record], effectiveFieldType)
            val implicitMapper = c.inferImplicitValue(expectedImplicitMapperType)
            if (!implicitMapper.equalsStructure(EmptyTree)) {
              assertIsJooqMeta(implicitMapper)
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

          val mapperRecordType = c.typecheck(implicitMapper).tpe.typeArgs(1)

          val toEntity = if (fieldIsOption) q"$implicitMapper.${TermName("toOptEntityAliased")}" else q"$implicitMapper.${TermName("toEntityAliased")}"

          if (mapperRecordType.equals(recordType)) {
            val toRecord = q"$implicitMapper.${TermName("toRecord")}"

            (q"$field = $toEntity(r, aliased)",
              if (fieldIsOption) {
                q"e.${TermName(fieldName)}.foreach(o => $toRecord(o, r))"
              } else {
                q"$toRecord(e.${TermName(fieldName)}, r)"
              },
              q"f = f ++ $implicitMapper.${TermName("fields")}",
              Nil)
          } else {
            val idSuffix = {
              val joinedTableType = implicitMapper.tpe.typeArgs.head
              if (tableMembers.get(s"${fieldName}Id").isDefined && joinedTableType.member(TermName("ID")).isTerm) {
                "id"
              } else if (tableMembers.get(s"${fieldName}Oid").isDefined && joinedTableType.member(TermName("OID")).isTerm) {
                "oid"
              } else {
                canNotFindMapIdFieldBetween(joinedTableType, s"$recordType.$fieldName")
              }
            }
            val tableFieldName = LOWER_CAMEL.to(UPPER_UNDERSCORE, s"$fieldName${idSuffix.capitalize}")
            val joinTableFieldName = LOWER_CAMEL.to(UPPER_UNDERSCORE, s"$idSuffix")
            val recordSetter = recordMembers.get(s"set${fieldName.capitalize}${idSuffix.capitalize}").get
            val fieldTypeIdMember = effectiveFieldType.member(TermName(idSuffix))
            val fieldTypeIdType = fieldTypeIdMember.asMethod.returnType
            val recordSetterType = recordSetter.typeSignature.paramLists.head.head.typeSignature
            val e2rTypeConversion = implicitConversion(from = fieldTypeIdType, to = recordSetterType)
            val nullInEntityMessage = s"$fieldName in entity ${entityType.typeSymbol.name.decodedName.toString} must not be null"
            val entityFieldConverted = q"$e2rTypeConversion(e.$fieldTermName.$fieldTypeIdMember)"

            val joinCondition = q"table.${TermName(tableFieldName)}.equal($implicitMapper.table.${TermName(joinTableFieldName)})"

            val join = if (fieldIsOption) {
              q"(current leftOuterJoin $implicitMapper.table).on($joinCondition)"
            } else {
              q"(if(leftJoin) (current leftOuterJoin $implicitMapper.table).on($joinCondition) else (current join $implicitMapper.table).on($joinCondition))"
            }
            val leftJoin = if (fieldIsOption) q"true" else q"leftJoin"

            (q"$field = $toEntity(r, aliased)",
              if (fieldIsOption) {
                q"e.${TermName(fieldName)}.foreach(o => r.$recordSetter($e2rTypeConversion(o.$fieldTypeIdMember)))"
              } else {
                q"r.$recordSetter(${checkNotNullTree(entityFieldConverted, nullInEntityMessage)})"
              },
              q"f = f ++ $implicitMapper.${TermName("fields")} ++ Array(table.${TermName(tableFieldName)})",
              q"current = $join" ::
                q"current = ($implicitMapper).joinedTable(current, $leftJoin)" :: Nil
              )
          }
      }
    }.unzip4


    val companion = entityType.typeSymbol.companion

    val code = q"""
      new ${weakTypeOf[JooqMeta[T, R, E]]} {
        override val table = $table
        override def joinedTable(t: ${weakTypeOf[Table[Record]]}, leftJoin: Boolean = false) = {
           var current = t
           ..${joins.flatten}
           current
        }
        override lazy val fields = {
          var f = Array.empty[${weakTypeOf[Field[_]]}]
          ..$mappedFields
          f
        }
        override def toEntityAliased(r: ${weakTypeOf[Record]}, aliased: Boolean = true) = $companion(..$toEntityParams)
        override def toRecord(e: $entityType, current: $recordType = null.asInstanceOf[$recordType])(implicit dsl: ${weakTypeOf[DSLContext]}): $recordType = {
          val r = if(current != null) current else dsl.newRecord(table)
          ..$toRecordParams
          r
        }
      }
    """


    c.Expr[JooqMeta[T, R, E]] {
      code
    }
  }

}
