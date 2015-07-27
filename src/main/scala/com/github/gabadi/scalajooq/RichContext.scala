package com.github.gabadi.scalajooq

import com.github.gabadi.scalajooq.JooqMacroMapper._
import com.google.common.base.CaseFormat._
import org.jooq.{Record, RecordMapper}

import scala.reflect.macros.TypecheckException

trait RichContext[C <: Context] {
  self: AbortContext[C] =>

  /** The macro context (of type `C`), must be provided by the inheritor */
  protected val context: C
  protected val namespace: Option[String]

  import context.universe._

  private val qPackage = q"com.github.gabadi.scalajooq"
  private val qJooqMeta = q"$qPackage.JooqMeta"

  def caseClassFields(caseClass: context.universe.Type) = {
    checkCaseClass(caseClass.typeSymbol.asClass)
    val declarations = caseClass.decls
    val ctor = declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get
    ctor.paramLists.head
  }

  def checkCaseClass(symbol: context.universe.ClassSymbol) = {
    if (!symbol.isClass || !symbol.isCaseClass)
      abortCanOnlyMapCaseClasses(symbol)(None)
  }

  def tableMembersMap(tableType: context.universe.Type) = tableType.members
    .filter { f =>
    val name = f.name.decodedName.toString
    name.toUpperCase.equals(name)
  }.map(m => tableMemberToName(m) -> m).toMap

  def tableMemberToName(symbol: context.universe.Symbol) = {
    val name = UPPER_UNDERSCORE.to(LOWER_CAMEL, symbol.name.decodedName.toString)
    namespace match {
      case Some(n) =>
        if (name.startsWith(n)) UPPER_CAMEL.to(LOWER_CAMEL, name.drop(n.length)) else name
      case None => name
    }
  }

  def recordMembersMap(recordType: context.universe.Type) = recordType.members
    .map(m => m.name.decodedName.toString -> m)
    .filter { case (n, m) =>
    n.startsWith("set") && m.asMethod.paramLists.head.size == 1
  }.toMap

  def tableInstanceMethod(tableType: context.universe.Type) = {
    val tableCompanion = tableType.typeSymbol.companion
    val companionMethod = TermName(LOWER_CAMEL.to(UPPER_UNDERSCORE, tableType.typeSymbol.name.decodedName.toString))
    q"$tableCompanion.$companionMethod"
  }


  def implicitConversion(from: Type, to: Type): context.Tree = {
    val r = context.inferImplicitValue(
      appliedType(typeOf[(_) => _].typeConstructor, from, to))
    if (r.equalsStructure(EmptyTree)) {
      abortCanNotFindImplicitConversion(from, to)(None)
    }
    r
  }

  def checkNotNullTree(tree: Tree, message: String) = q"$qPackage.Constraints.checkNotNull($tree, $message)"

  def treeToType(t: Tree) = {
    context.typecheck(q"$qPackage.JooqMeta.materializeType[$t]").tpe.resultType.typeArgs.head
  }

  def treeToTypeOpt(t: Tree) = {
    try {
      Some(context.typecheck(q"$qPackage.JooqMeta.materializeType[$t]").tpe.resultType.typeArgs.head)
    } catch {
      case e: TypecheckException => None
    }

  }

  def implicitJooqMeta(entityType: Type) = {
    val expectedImplicitMapperType = appliedType(typeOf[RecordMapper[_, _]].typeConstructor, typeOf[Record], entityType)
    val implicitMapper = context.inferImplicitValue(expectedImplicitMapperType)

    if (!implicitMapper.equalsStructure(EmptyTree)) {
      assertIsJooqMeta(implicitMapper)(None)
      Some(implicitMapper)
    } else {
      None
    }
  }

  def jooqMeta(tableType: Type, recordType: Type, entityType: Type) = {
    appliedType(typeOf[JooqMeta[_, _, _]].typeConstructor, tableType, recordType, entityType)
  }

  def fieldNameToTableMember(namespace: Option[String])(field: Symbol) = {
    LOWER_CAMEL.to(UPPER_UNDERSCORE, namespace.map(n => n + field.name.decodedName.toString.capitalize).getOrElse(field.name.decodedName.toString))
  }

  def existsMemberStartWith(lookupType: Type)(prefix: String) = lookupType.members.exists(_.name.decodedName.toString.startsWith(prefix))

  def guessJoinTableOpt(anyTable: Type, entity: Type): Option[Type] = {
    val fieldTypeName = entity.toString.split("\\.").last
    val upperUnderscoreFieldTypeName = LOWER_CAMEL.to(UPPER_UNDERSCORE, fieldTypeName)
    context.mirror.staticPackage(anyTable.toString.split("\\.").init.mkString(".")).typeSignature.members.collect{
      case c: ClassSymbol if c.name.decodedName.toString.equals(fieldTypeName) =>
        c.companion.typeSignature.member(TermName(upperUnderscoreFieldTypeName)).asMethod.returnType
    }.headOption
  }

}

object RichContext {
  type Context = scala.reflect.macros.blackbox.Context
}
