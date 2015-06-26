package com.github.gabadi.scalajooq

import db.test.public.Tables.FULL_USER
import db.test.public.tables.records.{FullUserRecord, UserWithAddressRecord, UserRecord}
import db.test.public.{Tables, tables}

import scala.reflect.runtime.{universe => ru}

class NoCaseProfile(name: Name)

case class FullUserNoCaseChild(id: Long, profile: NoCaseProfile, home: Location, work: Location)

class NamespaceTest extends BaseSpec {

  "Namespace" when {
    "fields" should {
      "be only mapped" in DB.withRollback { dsl =>
        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUser]

        mapper.fields should contain theSameElementsAs FULL_USER.ID :: FULL_USER.PROFILE_NAME_FIRST :: FULL_USER.PROFILE_NAME_LAST :: Nil
      }
    }
    "map record to entity" should {
      "map namespace values" in DB.withRollback { dsl =>

        val record = dsl.newRecord(Tables.USER_WITH_ADDRESS)
        record.setAddressNumber(1l)
        record.setAddressStreet("street")
        record.setAddressTelephone("tel")

        val mapper = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")

        val entity = mapper.toEntity(record)
        entity shouldBe Address("street", 1, "tel")
      }

      "map namespace values nested" in DB.withRollback { dsl =>

        val record = dsl.newRecord(Tables.USER_WITH_ADDRESS)
        record.setAddressNumber(1l)
        record.setAddressStreet("street")
        record.setAddressTelephone("tel")
        record.setId(2l)
        record.setFirstName("name")
        record.setLastName("last")

        implicit val addressMapping = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")
        val mapper = JooqMeta.metaOf[tables.UserWithAddress, UserWithAddressRecord, UserWithAddress]

        val entity = mapper.toEntity(record)
        entity shouldBe UserWithAddress(2l, "name", "last", Address("street", 1l, "tel"))
      }

      "map namespace values nested option present" in DB.withRollback { dsl =>

        val record = dsl.newRecord(Tables.USER_WITH_ADDRESS)
        record.setAddressNumber(1l)
        record.setAddressStreet("street")
        record.setId(2l)
        record.setFirstName("name")
        record.setLastName("last")
        record.setAddressTelephone("tel")

        implicit val addressMapping = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")
        val mapper = JooqMeta.metaOf[tables.UserWithAddress, UserWithAddressRecord, UserWithOptAddress]

        val entity = mapper.toEntity(record)
        entity shouldBe UserWithOptAddress(2l, "name", "last", Some(Address("street", 1l, "tel")))
      }

      "map namespace values nested option absent" in DB.withRollback { dsl =>

        val record = dsl.newRecord(Tables.USER_WITH_ADDRESS)
        record.setId(2l)
        record.setFirstName("name")
        record.setLastName("last")

        implicit val addressMapping = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")
        val mapper = JooqMeta.metaOf[tables.UserWithAddress, UserWithAddressRecord, UserWithOptAddress]

        val entity = mapper.toEntity(record)
        entity shouldBe UserWithOptAddress(2l, "name", "last", None)
      }

      "multi nesting" in DB.withRollback { dsl =>

        val record = dsl.newRecord(FULL_USER)
        record.setId(1l)
        record.setProfileNameFirst("name")
        record.setProfileNameLast("last")

        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUser]

        mapper.toEntity(record) shouldBe ProfileUser(1l, Profile(Name("name", "last")))
      }

      "multi nesting opt present" in DB.withRollback { dsl =>

        val record = dsl.newRecord(FULL_USER)
        record.setId(1l)
        record.setProfileNameFirst("name")
        record.setProfileNameLast("last")

        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUserOpt]

        mapper.toEntity(record) shouldBe ProfileUserOpt(1l, Some(Profile(Name("name", "last"))))
      }

      "multi nesting opt absent" in DB.withRollback { dsl =>

        val record = dsl.newRecord(FULL_USER)
        record.setId(1l)

        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUserOpt]

        mapper.toEntity(record) shouldBe ProfileUserOpt(1l, None)
      }

      "implicit embedded mapping" should {
        "map entity to record" in DB.withRollback { dsl =>

          val record = dsl.newRecord(FULL_USER)
          record.setId(1l)
          record.setProfileNameFirst("name")
          record.setProfileNameLast("last")
          record.setHomeAddressStreet("home")
          record.setHomeAddressTelephone("home")
          record.setHomeAddressNumber(2l)
          record.setWorkAddressStreet("work")
          record.setWorkAddressTelephone("work")
          record.setWorkAddressNumber(3l)

          val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, FullUser]
          val entity = mapper.toEntity(record)
          entity shouldBe FullUser(1, Profile(Name("name", "last")), Location(Address("home", 2, "home")), Location(Address("work", 3, "work")))
        }
      }
    }

    "map entity to record" should {
      "map namespace values" in DB.withRollback { implicit dsl =>

        val mapper = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")

        val record = mapper.toRecord(Address("street", 1, "tel"))

        record.getAddressNumber shouldBe 1l
        record.getAddressStreet shouldBe "street"
        record.getAddressTelephone shouldBe "tel"
      }

      "map namespace values nested" in DB.withRollback { implicit dsl =>

        implicit val addressMapping = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")
        val mapper = JooqMeta.metaOf[tables.UserWithAddress, UserWithAddressRecord, UserWithAddress]

        val record = mapper.toRecord(UserWithAddress(2l, "name", "last", Address("street", 1l, "tel")))
        record.getId shouldBe 2l
        record.getFirstName shouldBe "name"
        record.getLastName shouldBe "last"
        record.getAddressStreet shouldBe "street"
        record.getAddressNumber shouldBe 1l
        record.getAddressTelephone shouldBe "tel"
      }

      "map namespace values nested option present" in DB.withRollback { implicit dsl =>

        implicit val addressMapping = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")
        val mapper = JooqMeta.metaOf[tables.UserWithAddress, UserWithAddressRecord, UserWithOptAddress]
        val record = mapper.toRecord(UserWithOptAddress(2l, "name", "last", Some(Address("street", 1l, "tel"))))

        record.getId shouldBe 2l
        record.getFirstName shouldBe "name"
        record.getLastName shouldBe "last"
        record.getAddressStreet shouldBe "street"
        record.getAddressNumber shouldBe 1l
        record.getAddressTelephone shouldBe "tel"
      }

      "map namespace values nested option absent" in DB.withRollback { implicit dsl =>

        implicit val addressMapping = JooqMeta.namespacedMetaOf[tables.UserWithAddress, UserWithAddressRecord, Address]("address")
        val mapper = JooqMeta.metaOf[tables.UserWithAddress, UserWithAddressRecord, UserWithOptAddress]
        val record = mapper.toRecord(UserWithOptAddress(2l, "name", "last", None))

        record.getId shouldBe 2l
        record.getFirstName shouldBe "name"
        record.getLastName shouldBe "last"
        record.getAddressStreet shouldBe null
        record.getAddressNumber shouldBe null
        record.getAddressTelephone shouldBe null
      }

      "multi nesting" in DB.withRollback { implicit dsl =>
        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUser]
        val record = mapper.toRecord(ProfileUser(1l, Profile(Name("name", "last"))))

        record.getId shouldBe 1l
        record.getProfileNameFirst shouldBe "name"
        record.getProfileNameLast shouldBe "last"
      }

      "multi nesting opt present" in DB.withRollback { implicit dsl =>
        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUserOpt]
        val record = mapper.toRecord(ProfileUserOpt(1l, Some(Profile(Name("name", "last")))))

        record.getId shouldBe 1l
        record.getProfileNameFirst shouldBe "name"
        record.getProfileNameLast shouldBe "last"
      }

      "multi nesting opt absent" in DB.withRollback { implicit dsl =>
        implicit lazy val nameMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
        implicit lazy val profileMapping = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
        val mapper = JooqMeta.metaOf[tables.FullUser, FullUserRecord, ProfileUserOpt]
        val record = mapper.toRecord(ProfileUserOpt(1l, None))

        record.getId shouldBe 1l
        record.getProfileNameFirst shouldBe null
        record.getProfileNameLast shouldBe null
      }
    }
    "fails on" should {
      "no case class child" in DB.withRollback { dsl =>
        val code = s"com.github.gabadi.scalajooq.JooqMeta.metaOf[db.test.public.tables.FullUser, db.test.public.tables.records.FullUserRecord, com.github.gabadi.scalajooq.FullUserNoCaseChild]"
        assertNoCompiles(code, "Can only map case classes", "NoCaseProfile", "namespacedMetaOf", "FullUserNoCaseChild.profile", "(\"profile\")")
      }
    }
  }

}
