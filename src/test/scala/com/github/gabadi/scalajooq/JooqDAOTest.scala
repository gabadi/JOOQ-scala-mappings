package com.github.gabadi.scalajooq

import db.test.public.Tables.USER
import db.test.public.tables
import db.test.public.tables.records.UserRecord
import org.jooq.DSLContext

class JooqDAOTest extends BaseSpec {

  implicit val userMeta = JooqMeta.metaOf[tables.User, UserRecord, User]
  lazy val userDAO = new DefaultJooqDAO[UserRecord, User]() {}

  def insert(user: User)(implicit dsl: DSLContext) = {
    val id = userDAO.insert(user)
    user.copy(id = id)
  }

  "JooqDAO" when {
    "insert" should {
      "returns the generated id" in DB.withRollback { implicit dsl =>
        val toPersist = User(0l, "name", "last")
        val id = userDAO.insert(toPersist)
        id should be > 0l
      }
      "persist" in DB.withRollback { implicit dsl =>
        val toPersist = User(0l, "name", "last")
        val id = userDAO.insert(toPersist)
        val r = dsl.selectFrom(USER).fetchOne()
        r.getId shouldBe id
        r.getFirstName shouldBe toPersist.firstName
        r.getLastName shouldBe toPersist.lastName
      }
      "persist vararg" in DB.withRollback { implicit dsl =>
        userDAO.insert(User(0, "name1", "last1"), User(0, "name2", "last2"))
        val u1count = dsl.selectCount().from(USER).where(USER.FIRST_NAME equal "name1").and(USER.LAST_NAME equal "last1").fetchOne().value1()
        val u2count = dsl.selectCount().from(USER).where(USER.FIRST_NAME equal "name2").and(USER.LAST_NAME equal "last2").fetchOne().value1()

        u1count shouldBe 1
        u2count shouldBe 1
      }
    }

    "findAll" should {
      "empty result" in DB.withRollback { implicit dsl =>
        userDAO.findAll should be('empty)
      }
      "retrieve" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        userDAO.findAll should contain theSameElementsAs u1 :: u2 :: u3 :: Nil
      }
    }


    "insertAll" should {
      "persist list" in DB.withRollback { implicit dsl =>
        userDAO.insertAll(User(0, "name1", "last1") :: User(0, "name2", "last2") :: Nil)
        val u1count = dsl.selectCount().from(USER).where(USER.FIRST_NAME equal "name1").and(USER.LAST_NAME equal "last1").fetchOne().value1()
        val u2count = dsl.selectCount().from(USER).where(USER.FIRST_NAME equal "name2").and(USER.LAST_NAME equal "last2").fetchOne().value1()

        u1count shouldBe 1
        u2count shouldBe 1
      }
    }

    "update" should {
      "return 1 if entity updated" in DB.withRollback { implicit dsl =>
        val user = User(0l, "name", "last")
        val id = userDAO.insert(user)
        val affected = userDAO.update(user.copy(lastName = "last2", id = id))
        affected shouldBe 1
      }
      "return 1 if entity not updated" in DB.withRollback { implicit dsl =>
        val user = insert(User(0l, "name", "last"))
        val affected = userDAO.update(user)
        affected shouldBe 1
      }
      "update entity values" in DB.withRollback { implicit dsl =>
        val user = User(0l, "name", "last")
        val id = userDAO.insert(user)
        val userCopy = user.copy(lastName = "last2", id = id)
        userDAO.update(userCopy)
        dsl.selectFrom(USER).fetchOne(userMeta) shouldBe userCopy
      }
      "ignore and return 0 if entity does not exist" in DB.withRollback { implicit dsl =>
        val effected = userDAO.update(User(0l, "name", "last"))
        effected shouldBe 0
        dsl.selectCount().from(USER).fetchOne().value1() shouldBe 0
      }
      "var args return affected and update entities" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = User(0, "name3", "last3")
        val u2Copy = u2.copy(firstName = "nameCopy")

        val effected = userDAO.update(u1, u2Copy, u3)
        effected shouldBe 2
        userDAO.findAll should contain theSameElementsAs u1 :: u2Copy :: Nil
      }
    }

    "updateAll" should {
      "return affected and update entities" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = User(0, "name3", "last3")
        val u2Copy = u2.copy(firstName = "nameCopy")

        val effected = userDAO.updateAll(u1 :: u2Copy :: u3 :: Nil)
        effected shouldBe 2
        userDAO.findAll should contain theSameElementsAs u1 :: u2Copy :: Nil
      }
    }

    "findById" should {
      "retrieve matching entities" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))

        userDAO.findById(u1.id) shouldBe Some(u1)
        userDAO.findById(u2.id) shouldBe Some(u2)
        userDAO.findById(u2.id + 1) shouldBe None
      }

      "retrieve matching entities varargs" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))

        userDAO.findByIds(u1.id :: u2.id :: u2.id + 1 :: Nil) should contain theSameElementsAs u1 :: u2 :: Nil
      }
    }

    "findByIds" should {
      "retrieve matching entities varargs" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))

        userDAO.findByIds(u1.id, u2.id, u2.id + 1) should contain theSameElementsAs u1 :: u2 :: Nil
      }

      "retrieve matching entities list" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))

        userDAO.findByIds(u1.id :: u2.id :: u2.id + 1 :: Nil) should contain theSameElementsAs u1 :: u2 :: Nil
      }
    }

    "delete" should {
      "ignore not exists entity" in DB.withRollback { implicit dsl =>
        val u = insert(User(0, "name1", "last1"))
        val affected = userDAO.delete(u.copy(id = u.id + 1))
        affected shouldBe 0
        userDAO.findAll should contain theSameElementsAs u :: Nil
      }
      "delete entity" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        userDAO.delete(u1)
        userDAO.findAll should contain theSameElementsAs u2 :: u3 :: Nil
      }

      "delete entities varargs" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        val affected = userDAO.delete(u1, u2, u3.copy(id = u3.id + 1))
        affected shouldBe 2
        userDAO.findAll should contain theSameElementsAs u3 :: Nil
      }
    }

    "deleteAll" should {
      "delete entities list" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        val affected = userDAO.deleteAll(u1 :: u2 :: u3.copy(id = u3.id + 1) :: Nil)
        affected shouldBe 2
        userDAO.findAll should contain theSameElementsAs u3 :: Nil
      }

      "delete whole table" in DB.withRollback { implicit dsl =>
        insert(User(0, "name1", "last1"))
        insert(User(0, "name2", "last2"))
        insert(User(0, "name3", "last3"))

        val affected = userDAO.deleteAll
        affected shouldBe 3
        userDAO.findAll should contain theSameElementsAs Nil
      }
    }

    "deleteById" should {
      "ignore not existing entity" in DB.withRollback { implicit dsl =>
        val u = insert(User(0, "name1", "last1"))
        val affected = userDAO.deleteById(u.id + 1)
        affected shouldBe 0
        userDAO.findAll should contain theSameElementsAs u :: Nil
      }
      "delete entity" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        userDAO.deleteById(u1.id)
        userDAO.findAll should contain theSameElementsAs u2 :: u3 :: Nil
      }

      "delete entities varargs" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        val affected = userDAO.deleteById(u1.id, u2.id, u3.id + 1)
        affected shouldBe 2
        userDAO.findAll should contain theSameElementsAs u3 :: Nil
      }
    }

    "deleteByIds" should {
      "delete entities list" in DB.withRollback { implicit dsl =>
        val u1 = insert(User(0, "name1", "last1"))
        val u2 = insert(User(0, "name2", "last2"))
        val u3 = insert(User(0, "name3", "last3"))

        val affected = userDAO.deleteByIds(u1.id :: u2.id :: u3.id + 1 :: Nil)
        affected shouldBe 2
        userDAO.findAll should contain theSameElementsAs u3 :: Nil
      }
    }

  }
}
