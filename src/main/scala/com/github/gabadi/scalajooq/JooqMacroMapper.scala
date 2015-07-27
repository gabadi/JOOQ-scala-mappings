package com.github.gabadi.scalajooq

import com.google.common.base.CaseFormat.{LOWER_CAMEL, LOWER_UNDERSCORE, UPPER_UNDERSCORE}
import org.jooq.{DSLContext, Field, Record, Table}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.{Failure, Success}

trait JooqMacroMapper[C <: Context] {

  val rc = {
    val n = namespace
    new {
      val context: co.type = co
      val namespace = n
    } with RichContext[co.type] with AbortContext[co.type]
  }
  protected val co: C

  import co.universe._

  protected val namespace: Option[String]

  def tryGenerateByTree(tableType: Tree, recordType: Tree, entityType: Tree): scala.util.Try[Tree] = {
    tryGenerateByType(
      tableType = rc.treeToType(tableType),
      recordType = rc.treeToType(recordType),
      entityType = rc.treeToType(entityType)
    )
  }

  def childOf(c: C, namespace: Option[String]) = {
    val n = namespace
    new {
      val co: c.type = c
      val namespace = n
    } with JooqMacroMapper[c.type]
  }

  def tryGenerateByType(tableType: Type, recordType: Type, entityType: Type): scala.util.Try[Tree] = {
    scala.util.Try {
      val fields = rc.caseClassFields(entityType)
      val table = rc.tableInstanceMethod(tableType)

      val tableMembers = rc.tableMembersMap(tableType)
      val recordMembers = rc.recordMembersMap(recordType)
      val (toEntityParams, toRecordParams, metaFields, mappedFields, joins, embeddedEntities) =
        fields.map { field =>
          val fieldName = field.name.decodedName.toString
          val fieldTermName = field.asTerm.name
          val columnName = LOWER_CAMEL.to(UPPER_UNDERSCORE, fieldName)
          val columnNameNoNamespace = namespace.map(n => columnName.replaceFirst(LOWER_CAMEL.to(UPPER_UNDERSCORE, n) + "_", "")).getOrElse(columnName)
          val fieldNameLowerUnderscore = LOWER_CAMEL.to(LOWER_UNDERSCORE, fieldName)
          val fieldIsOption = field.typeSignature <:< typeOf[Option[_]]
          val effectiveFieldType = if (fieldIsOption) field.typeSignature.typeArgs.head else field.typeSignature
          tableMembers.get(fieldName) match {
            // direct matching between record and entity
            case Some(recordMember) =>
              recordMembers.get(s"set${namespace.getOrElse("").capitalize}${fieldName.capitalize}").get
              val recordSetter = recordMembers.get(s"set${namespace.getOrElse("").capitalize}${fieldName.capitalize}").get

              val recordFieldType = recordSetter.asMethod.paramLists.head.head.typeSignature

              val e2rTypeConversion = rc.implicitConversion(from = effectiveFieldType, to = recordFieldType)
              val r2eTypeConversion = rc.implicitConversion(from = recordFieldType, to = effectiveFieldType)

              val getValue = q"r.getValue(table.$recordMember)"

              if ((r2eTypeConversion equalsStructure EmptyTree) || (e2rTypeConversion equalsStructure EmptyTree)) {
                rc.abortCanNotFindImplicitConversion(s"$entityType.$fieldName($effectiveFieldType)", s"$recordType.$columnName($recordFieldType)")(None)
              } else {
                if (fieldIsOption) {
                  (q"$fieldTermName = Option($getValue).map($r2eTypeConversion)",
                    q"r.$recordSetter(e.$fieldTermName.map($e2rTypeConversion).orNull[$recordFieldType])",
                    q"lazy val ${TermName(columnName)} = table.$recordMember",
                    q"f = f ++ Array(table.$recordMember)",
                    Nil,
                    None)
                } else {
                  val nullInRecordMessage = s"${recordMember.name.decodedName.toString} in record ${recordType.typeSymbol.name.decodedName.toString} is null in the database. This is inconsistent"
                  val nullInEntityMessage = s"$fieldName in entity ${entityType.typeSymbol.name.decodedName.toString} must not be null"
                  val entityFieldConverted = q"$e2rTypeConversion(e.$fieldTermName)"
                  (q"$fieldTermName = $r2eTypeConversion(${rc.checkNotNullTree(getValue, nullInRecordMessage)})",
                    q"r.$recordSetter(${rc.checkNotNullTree(entityFieldConverted, nullInEntityMessage)})",
                    q"lazy val ${TermName(columnName)} = table.$recordMember",
                    q"f = f ++ Array(table.$recordMember)",
                    Nil,
                    None)
                }
              }
            case None =>
              rc.guessJoinTableOpt(tableType, effectiveFieldType) map { joinTable =>
                // try to join
                val joinRecordType = joinTable.decl(TermName("getRecordType")).asMethod.returnType.typeArgs.head
                childOf(co, namespace).tryGenerateByType(tableType = joinTable, recordType = joinRecordType, entityType = effectiveFieldType) match {
                  case Success(child) =>
                    val objectName = s"O${fieldName.capitalize.drop(namespace.getOrElse("").length)}"
                    val objectTermName = TermName(objectName)
                    val childObject = q"object $objectTermName extends ${rc.jooqMeta(joinTable, joinRecordType, effectiveFieldType)} {..$child}"

                    val idSuffix = {
                      val maybeMappedMethods = tableMembers.keySet.filter(f => f.startsWith(fieldName))
                      if (maybeMappedMethods.isEmpty) {
                        rc.abortCanNotFindMapIdFieldBetween(joinTable, s"$entityType.$fieldName")(None)
                      } else if (maybeMappedMethods.size == 1) {
                        val suffix = maybeMappedMethods.head.replaceFirst(fieldName, "")
                        s"${suffix.substring(0, 0).toLowerCase}${suffix.substring(1, suffix.length)}"
                      } else {
                        if (maybeMappedMethods.exists(_.equals(fieldName + "Id"))) {
                          "id"
                        } else if (maybeMappedMethods.exists(_.equals(fieldName + "Oid"))) {
                          "oid"
                        } else if (maybeMappedMethods.exists(_.equals(fieldName + "Code"))) {
                          "code"
                        } else {
                          // improve message
                          rc.abortCanNotFindMapIdFieldBetween(joinTable, s"$entityType.$fieldName")(None)
                        }

                      }
                    }

                    val tableFieldName = LOWER_CAMEL.to(UPPER_UNDERSCORE, s"$fieldName${idSuffix.capitalize}")
                    val joinTableFieldName = LOWER_CAMEL.to(UPPER_UNDERSCORE, s"$idSuffix")
                    val recordSetter = recordMembers.get(s"set${fieldName.capitalize}${idSuffix.capitalize}").get
                    val fieldTypeIdMember = effectiveFieldType.member(TermName(idSuffix))
                    val fieldTypeIdType = fieldTypeIdMember.asMethod.returnType
                    val recordSetterType = recordSetter.typeSignature.paramLists.head.head.typeSignature
                    val e2rTypeConversion = rc.implicitConversion(from = fieldTypeIdType, to = recordSetterType)
                    val nullInEntityMessage = s"$fieldName in entity ${entityType.typeSymbol.name.decodedName.toString} must not be null"
                    val entityFieldConverted = q"$e2rTypeConversion(e.$fieldTermName.$fieldTypeIdMember)"
                    val toEntity = if (fieldIsOption) q"$objectTermName.${TermName("toOptEntity")}" else q"$objectTermName.${TermName("toEntity")}"


                    val newNamespace = q"""namespace.map(n => n + "_" + $fieldNameLowerUnderscore).getOrElse($fieldNameLowerUnderscore)"""

                    //val joinTable = q"$implicitMapper.table.as($namespace)"
                    val joinTable2 = q"$objectTermName.${TermName("table")}"

                    //val ownTable = q"namespace.map(n => table.as(n)).getOrElse(table)"
                    val ownTable = q"table"

                    val joinCondition = q"$ownTable.${TermName(tableFieldName)}.equal($joinTable2.${TermName(joinTableFieldName)})"

                    val join = if (fieldIsOption) {
                      q"(current leftOuterJoin $joinTable2).on($joinCondition)"
                    } else {
                      q"(if(leftJoin) (current leftOuterJoin $joinTable2).on($joinCondition) else (current join $joinTable2).on($joinCondition))"
                    }
                    val leftJoin = if (fieldIsOption) q"true" else q"leftJoin"

                    (q"$field = $toEntity(r)",
                      if (fieldIsOption) {
                        q"e.${TermName(fieldName)}.foreach(o => r.$recordSetter($e2rTypeConversion(o.$fieldTypeIdMember)))"
                      } else {
                        q"r.$recordSetter(${rc.checkNotNullTree(entityFieldConverted, nullInEntityMessage)})"
                      },
                      q"",
                      q"f = f ++ ${TermName(objectName)}.${TermName("fields")} ++ Array(table.${TermName(tableFieldName)})",
                      q"current = $join" ::
                        q"current = ($objectTermName).joinedTable(current, Some($newNamespace), $leftJoin)" :: Nil,
                      Some(childObject)
                      )

                  case Failure(e: AbortException) =>
                    rc.abortFieldCanNotBeMapped(s"$entityType.$fieldName")(Some(e.getMessage))
                  case Failure(e) =>
                    throw e

                }
              } getOrElse {
                // try to embedd
                val newNamespace = namespace.map(n => s"$n${fieldName.capitalize}").getOrElse(fieldName)
                val newNamespaceUpper = LOWER_CAMEL.to(UPPER_UNDERSCORE, newNamespace)
                val mayExistNamespace = rc.existsMemberStartWith(tableType)(newNamespaceUpper)
                if (!mayExistNamespace) {
                  rc.abortFieldNotFoundInRecord(s"$entityType.$fieldName", s"$recordType.$newNamespaceUpper")(None)
                } else {
                  childOf(co, Some(newNamespace)).tryGenerateByType(tableType = tableType, recordType = recordType, entityType = effectiveFieldType) match {
                    case Success(child) =>
                      val objectName = s"O${newNamespace.capitalize.drop(namespace.getOrElse("").length)}"
                      val objectTermName = TermName(objectName)
                      val childObject = q"object $objectTermName extends ${rc.jooqMeta(tableType, recordType, effectiveFieldType)} {..$child}"
                      val toEntity = if (fieldIsOption) q"$objectTermName.${TermName("toOptEntity")}" else q"$objectTermName.${TermName("toEntity")}"

                      val toRecord = q"$objectTermName.${TermName("toRecord")}"
                      val fieldToRecord = if (fieldIsOption) {
                        q"e.${TermName(fieldName)}.foreach(o => $toRecord(o, r))"
                      } else {
                        q"$toRecord(e.${TermName(fieldName)}, r)"
                      }

                      (q"$field = $toEntity(r)",
                        fieldToRecord,
                        q"lazy val ${TermName(columnNameNoNamespace)} = $objectTermName",
                        q"f = f ++ $objectTermName.${TermName("fields")}",
                        Nil,
                        Some(childObject))
                    case Failure(e: AbortException) =>
                      rc.abortFieldCanNotBeMapped(s"$entityType.$fieldName")(Some(e.getMessage))
                    case Failure(e) =>
                      throw e
                  }
                }
              }
          }
        }.unzip6

      val companion = entityType.typeSymbol.companion

      val body = q"""
        ..${embeddedEntities.flatten}
        override val table = $table
        override def joinedTable(t: ${weakTypeOf[Table[Record]]}, namespace: Option[String] = None, leftJoin: Boolean = false) = {
           var current = t
           ..${joins.flatten}
           current
        }
        ..$metaFields
        override lazy val fields = {
          var f = Array.empty[${weakTypeOf[Field[_]]}]
          ..$mappedFields
          f
        }
        override def toEntity(r: ${weakTypeOf[Record]}) = $companion(..$toEntityParams)
        override def toRecord(e: $entityType, current: $recordType = null.asInstanceOf[$recordType])(implicit dsl: ${weakTypeOf[DSLContext]}): $recordType = {
          val r = if(current != null) current else dsl.newRecord(table)
          ..$toRecordParams
          r
        }
    """

      body
    }
  }

}

object JooqMacroMapper {
  type Context = scala.reflect.macros.blackbox.Context
}
