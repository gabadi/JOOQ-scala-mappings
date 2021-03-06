package com.github.gabadi.scalajooq

import db.test.public.Tables.USER
import db.test.public.tables
import db.test.public.tables.records.UserRecord


class UserNoCase(id: Long, firstName: String, lastName: String)

case class UserRecordAbsent(id2: Long, firstName: String, lastName: String)

case class UserDiffType(id: String, firstName: String, lastName: String)

case class UserNoName(id: Long, lastName: String)

case class UserOption(id: Long, firstName: Option[String], lastName: Option[String])

class BasicMappingTest extends BaseSpec {

  "BasicMapping" when {
    "fields" should {
      "support all" in DB.withRollback { dsl =>
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, User]
        UserMapper.fields should contain theSameElementsAs (USER.ID :: USER.FIRST_NAME :: USER.LAST_NAME :: Nil)
      }
      "only have fields presents in the entity" in DB.withRollback { dsl =>
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, UserNoName] {
        }
        UserMapper.fields should contain theSameElementsAs (USER.ID :: USER.LAST_NAME :: Nil)
      }
    }
    "map record to entity" should {
      "map record" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        r.setId(1l)
        r.setFirstName("name")
        r.setLastName("last")
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, User] {
        }
        UserMapper.map(r) shouldBe User(1l, "name", "last")
      }
      "record to entity" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        r.setId(1l)
        r.setFirstName("name")
        r.setLastName("last")
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, User] {
        }
        UserMapper.toEntity(r) shouldBe User(1l, "name", "last")
      }
      "record to entity missing field" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        r.setId(1l)
        r.setFirstName("name")
        r.setLastName("last")
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, UserNoName] {
        }
        UserMapper.toEntity(r) shouldBe UserNoName(1l, "last")
      }
      "record to opt entity present" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        r.setId(1l)
        r.setFirstName("name")
        r.setLastName("last")
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, User] {
        }
        UserMapper.toOptEntity(r) shouldBe Some(User(1l, "name", "last"))
      }
      "record to entity opt field absent" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        r.setId(1l)
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, UserOption] {
        }
        UserMapper.toOptEntity(r) shouldBe Some(UserOption(1, None, None))
      }
      "record to entity opt field present" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        r.setId(1l)
        r.setFirstName("name")
        r.setLastName("last")
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, UserOption] {
        }
        UserMapper.toOptEntity(r) shouldBe Some(UserOption(1, Some("name"), Some("last")))
      }
      "record to opt entity absent" in DB.withRollback { dsl =>
        val r = dsl.newRecord(USER)
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, User] {
        }
        UserMapper.toOptEntity(r) shouldBe None
      }
    }
    "map entity to record" should {
      "map record" in DB.withRollback { implicit dsl =>
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, User] {
        }
        val r = UserMapper.toRecord(User(1l, "name", "last"))
        r.getId shouldBe 1l
        r.getFirstName shouldBe "name"
        r.getLastName shouldBe "last"
      }
      "entity to record missing field" in DB.withRollback { implicit dsl =>
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, UserNoName] {
        }
        val r = UserMapper toRecord UserNoName(1l, "last")
        r.getId shouldBe 1l
        r.getFirstName shouldBe null
        r.getLastName shouldBe "last"
      }
      "entity to record opt field absent" in DB.withRollback { implicit dsl =>
        @meta implicit object UserMapper extends JooqMeta[tables.User, UserRecord, UserOption] {
        }
        val r = UserMapper toRecord UserOption(1l, None, None)
        r.getId shouldBe 1l
        r.getFirstName shouldBe null
        r.getLastName shouldBe null
      }
      "record to entity opt field present" in DB.withRollback { implicit dsl =>
        @meta object UserMapper extends JooqMeta[tables.User, UserRecord, UserOption] {
        }
        val r = UserMapper toRecord UserOption(1, Some("name"), Some("last"))
        r.getId shouldBe 1l
        r.getFirstName shouldBe "name"
        r.getLastName shouldBe "last"
      }
    }
    "fails on" should {
      "no case class" in DB.withRollback { dsl =>
        val code = s"com.github.gabadi.scalajooq.JooqMeta.metaOf[db.test.public.tables.User, db.test.public.tables.records.UserRecord, com.github.gabadi.scalajooq.UserNoCase]"
        assertNoCompiles(code, "only case classes are supported")
      }
      "different field type" in DB.withRollback { dsl =>
        val code = s"com.github.gabadi.scalajooq.JooqMeta.metaOf[db.test.public.tables.User, db.test.public.tables.records.UserRecord, com.github.gabadi.scalajooq.UserDiffType]"
        assertNoCompiles(code, "can not find an implicit conversion from")
      }
      "entity field absent in record" in DB.withRollback { dsl =>
        val code = s"com.github.gabadi.scalajooq.JooqMeta.metaOf[db.test.public.tables.User, db.test.public.tables.records.UserRecord, com.github.gabadi.scalajooq.UserRecordAbsent]"
        assertNoCompiles(code, "ID2 column, but doesn't exists")
      }
    }
  }
}
