package com.github.gabadi.scalajooq

import db.test.public.Tables.{CITY, COUNTRY, STATE}
import db.test.public.tables
import db.test.public.tables.records.{RoadRecord, CityRecord, CountryRecord, StateRecord}
import org.jooq.{Record2, DSLContext}

case class City(id: Long, name: String, state: State)

case class CityOptState(id: Long, name: String, state: Option[State])

case class State(id: Long, name: String, country: Country)

case class Country(id: Long, name: String)

case class Road(fromCity: CityOptState, toCity: CityOptState)

class JoinTest extends BaseSpec {

  "Join" when {
    "selectTable" should {
      "has the correct inner join" in DB.withRollback { dsl =>
        implicit lazy val countryMeta = JooqMeta.metaOf[tables.Country, CountryRecord, Country]
        implicit lazy val stateMeta = JooqMeta.metaOf[tables.State, StateRecord, State]
        implicit lazy val cityMeta = JooqMeta.metaOf[tables.City, CityRecord, City]

        cityMeta.selectTable.toString shouldBe (CITY join STATE).on(CITY.STATE_ID equal STATE.ID).join(COUNTRY).on(STATE.COUNTRY_ID equal COUNTRY.ID).toString
      }

      "has the correct outer join" in DB.withRollback { dsl =>
        implicit lazy val countryMeta = JooqMeta.metaOf[tables.Country, CountryRecord, Country]
        implicit lazy val stateMeta = JooqMeta.metaOf[tables.State, StateRecord, State]
        implicit lazy val cityOptMeta = JooqMeta.metaOf[tables.City, CityRecord, CityOptState]

        cityOptMeta.selectTable.toString shouldBe (CITY leftOuterJoin STATE).on(CITY.STATE_ID equal STATE.ID).leftOuterJoin(COUNTRY).on(STATE.COUNTRY_ID equal COUNTRY.ID).toString
      }
    }

    class JoinDAOs(
                    val countries: DefaultJooqDAO[CountryRecord, Country],
                    val states: DefaultJooqDAO[StateRecord, State],
                    val cities: DefaultJooqDAO[CityRecord, City],
                    val citiesOptState: DefaultJooqDAO[CityRecord, CityOptState],
                    val roads: GenericJooqDAO[RoadRecord, Record2[Long, Long], Road]
                    ) {
      def insertCountry(c: Country)(implicit dsl: DSLContext) = {
        val id = countries.insert(c)
        c.copy(id = id)
      }

      def insertState(s: State)(implicit dsl: DSLContext) = {
        val id = states.insert(s)
        s.copy(id = id)
      }

      def insertCity(c: City)(implicit dsl: DSLContext) = {
        val id = cities.insert(c)
        c.copy(id = id)
      }

      def insertCityOptState(c: CityOptState)(implicit dsl: DSLContext) = {
        val id = citiesOptState.insert(c)
        c.copy(id = id)
      }
    }

    lazy val joinDAOs = {
      implicit val countryMeta = JooqMeta.metaOf[tables.Country, CountryRecord, Country]
      implicit val stateMeta = JooqMeta.metaOf[tables.State, StateRecord, State]
      implicit val cityMeta = JooqMeta.metaOf[tables.City, CityRecord, City]
      implicit val cityOptStateMeta = JooqMeta.metaOf[tables.City, CityRecord, CityOptState]
      implicit val roadMeta = JooqMeta.metaOf[tables.Road, RoadRecord, Road]

      val countryDAO = new DefaultJooqDAO[CountryRecord, Country]() {}
      val stateDAO = new DefaultJooqDAO[StateRecord, State]() {}
      val cityDAO = new DefaultJooqDAO[CityRecord, City]() {}
      val cityOptStateDAO = new DefaultJooqDAO[CityRecord, CityOptState]() {}
      val roadDAO = new GenericJooqDAO[RoadRecord, Record2[Long, Long], Road]() {}
      new JoinDAOs(countryDAO, stateDAO, cityDAO, cityOptStateDAO, roadDAO)
    }

    "dao" should {
      "manages inner join relations" in DB.withRollback { implicit dsl =>
        val co = joinDAOs.insertCountry(Country(0, "c"))
        val s1 = joinDAOs.insertState(State(0, "s1", co))
        val s2 = joinDAOs.insertState(State(0, "s2", co))
        val ci1 = joinDAOs.insertCity(City(0, "c1", s1))
        val ci2 = joinDAOs.insertCity(City(0, "c2", s1))
        val ci3 = joinDAOs.insertCity(City(0, "c3", s2))
        val ci4 = joinDAOs.insertCity(City(0, "c4", s2))

        joinDAOs.countries.findAll should contain theSameElementsAs co :: Nil
        joinDAOs.states.findAll should contain theSameElementsAs s1 :: s2 :: Nil
        joinDAOs.cities.findAll should contain theSameElementsAs ci1 :: ci2 :: ci3 :: ci4 :: Nil
        ci1.name shouldBe "c1"
        ci2.name shouldBe "c2"
        ci3.name shouldBe "c3"
        ci4.name shouldBe "c4"

        s1.name shouldBe "s1"
        s2.name shouldBe "s2"

        co.name shouldBe "c"
      }

      "manages left outer join relations" in DB.withRollback { implicit dsl =>
        val co = joinDAOs.insertCountry(Country(0, "c"))
        val s1 = joinDAOs.insertState(State(0, "s1", co))
        val s2 = joinDAOs.insertState(State(0, "s2", co))
        val ci1 = joinDAOs.insertCityOptState(CityOptState(0, "c1", Some(s1)))
        val ci2 = joinDAOs.insertCityOptState(CityOptState(0, "c2", None))
        val ci3 = joinDAOs.insertCityOptState(CityOptState(0, "c3", Some(s2)))
        val ci4 = joinDAOs.insertCityOptState(CityOptState(0, "c4", None))

        joinDAOs.countries.findAll should contain theSameElementsAs co :: Nil
        joinDAOs.states.findAll should contain theSameElementsAs s1 :: s2 :: Nil
        joinDAOs.citiesOptState.findAll should contain theSameElementsAs ci1 :: ci2 :: ci3 :: ci4 :: Nil
        ci1.name shouldBe "c1"
        ci2.name shouldBe "c2"
        ci3.name shouldBe "c3"
        ci4.name shouldBe "c4"

        s1.name shouldBe "s1"
        s2.name shouldBe "s2"

        co.name shouldBe "c"
      }
    }

    "compositeId" should {
      "support DAO insert" in DB.withRollback { implicit dsl =>
        val c1 = joinDAOs.insertCityOptState(CityOptState(0, "name1", None))
        val c2 = joinDAOs.insertCityOptState(CityOptState(0, "name2", None))
        val id = joinDAOs.roads.insert(Road(c1, c2))
        id.getValue(0) shouldBe c1.id
        id.getValue(1) shouldBe c2.id
      }
      "support DAO find" in DB.withRollback { implicit dsl =>
        val c1 = joinDAOs.insertCityOptState(CityOptState(0, "name1", None))
        val c2 = joinDAOs.insertCityOptState(CityOptState(0, "name2", None))
        joinDAOs.roads.insert(Road(c1, c2))
        joinDAOs.roads.findAll should contain theSameElementsAs Road(c1, c2) :: Nil
      }
    }
  }

}
