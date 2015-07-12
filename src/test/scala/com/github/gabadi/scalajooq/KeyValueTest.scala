package com.github.gabadi.scalajooq

import db.test.public.tables
import db.test.public.tables.records.KeyValueRecord


case class KeyValue(key: String, value: String)

class KeyValueTest extends BaseSpec {

  "KeyValue" when {
    "dao" should {
      "support insert and select" in DB.withRollback { implicit dsl =>
        implicit val keyValueMeta = JooqMeta.metaOf[tables.KeyValue, KeyValueRecord, KeyValue]
        val dao: JooqDAO[KeyValueRecord, String, KeyValue] = new JooqDAO[KeyValueRecord, String, KeyValue]()(keyValueMeta) {}
        val key = dao.insert(KeyValue("1", "2"))
        key shouldBe "1"
        dao.findAll should contain theSameElementsAs KeyValue("1", "2") :: Nil
      }
      }
    }
}
