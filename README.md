# JOOQ-scala-mappingss

All the [JOOQ](http://www.jooq.org) power, in the [scala](http://www.scala-lang.org/) world.

### Why?
-----------

JOOQ is a great tool for accessing a SQL store. It's expressive, powerful, etc. The single thing that i would like to improve is to facilitate the most simple use cases (more or less like an ORM does). And for that this project uses scala macros.

### Installation
------------

TODO: Publish maven central
TODO: Depends on jooq 3.6.1 or greater with scala codegen

### Functionalities
---------------

 1. [Record to entities mappings](#record-to-entity-mapping)
 2. [Entity to record mappings](#entity-to-record-mapping)
 3. [Scala option support](#scala-option-support)
 4. [Implicit type conversion](#implicit-type-conversion)
 5. [Embedded entities](#embedded-entities)
 6. [Base DAO functionality](#base-dao-functionality)
 7. [Base query generator](#base-query-generator)
 8. [One to one / one to many support](#one-to-one-one-to-many-support)

More to come...

### Record to entity mapping

Given this mysql table:
```
CREATE TABLE `user` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `first_name` varchar(50) NOT NULL,
  `last_name` varchar(50) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```
If i want to map this with my case class
```
case class User(id: java.lang.Long,
                firstName: String, 
                lastName: String) {
}
```
That is as simple as doing:
```
    val r = dsl.newRecord(Tables.USER)
    r setId 1l
    r setFirstName "name"
    r setLastName "last"

    val meta = com.github.gabadi.scalajooq.JooqMeta.metaOf[tables.User, UserRecord, User]
    
    val user = meta toEntity r

```
Note: **tables.User** and **UserRecord** were autogenerated by the jooq scala codegen.

Note: To generate the dsl instance, see the JOOQ documentation

This is not magic, this is not reflections, this is a macro.
But what do i win? 
The answer is that now, all the runtime exceptions will be compilation exceptions. So, if for example, you have a bug in the user like this:

```
case class User(id: java.lang.Long,
                firsName: String, 
                lastName: String) {
}
```
During compilation you'll see a message like this:

```
Mappings error:
 com.github.gabadi.scalajooq.User.firsName expects a db.test.public.tables.records.UserRecord.FIRS_NAME column, but doesn't exists
```

### Entity to record mapping
Now in the other way this is valid too
```
    val entity = User(0, "name", "last")
    
    val meta = com.github.gabadi.scalajooq.JooqMeta.metaOf[tables.User, UserRecord, User]
    
    val record = meta toRecord entity
    record.store()

```
Note: This the method toRecord depends in an implicit DSLContext instance

### Scala option support
One important feature in scala, that it's necessary yo wait till JOOQ version 4.0 for a correct support are Option values.
But with JOOQ-scala-mapping, giving this class:
```
case class UserOption(id: java.lang.Long,
                      firstName: Option[String], 
                      lastName: Option[String]) {
}
```
Anyone can make a mapping like this:

```
    val r = dsl.newRecord(Tables.USER)
    r setId 1l

    val meta = com.github.gabadi.scalajooq.JooqMeta.metaOf[tables.User, UserRecord, User]
    
    val user = meta toEntity r
    println(user)
```
With this result:
```
    User(1, None, None)
```
And in the other hand, JOOQ-scala-mappings will never let you have an Entity with a null field. Instead of that, an exception will be thrown when you try to generate that entity.

###Implicit type conversion
The current user as we have defined it, has a little issue. If you see the id field, you'll see that it's not a scala Long, it's a java Long.
So, what if you want a standard scala **Long**?, or **Int**? or **Boolean**?
For that, you have implicit conversions. What that means is simple that for **JOOQ-scala-mappings** this two entities are valid representations for the **user** table

```
case class User1(id: java.lang.Long,
                 firsName: String, 
                 lastName: String) {
}
case class User2(id: Long,
                 firsName: String, 
                 lastName: String) {
}

```

###Embedded entities
What if i want to represent a complex entity, with some other embedded entity inside.
Something like this:

```
case class Name(first: String, last: String)

case class Profile(name: Name)

case class FullUser(id: Long, profile: Profile)
```

This can be automatically mapped to a table like this:
```
CREATE TABLE `full_user` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `profile_name_first` varchar(50),
  `profile_name_last` varchar(50),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```
Where we can see that the **FullUser** has an **Id** and we can call the **Profile** an embedded entity inside the **FullUser**, and the **Name** inside the **Profile** may be another embedded entity. <br/>
And that hierarchy can be mapped with something like this:
```
 val nameMeta = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Name]("profileName")
 val profileMeta = JooqMeta.namespacedMetaOf[tables.FullUser, FullUserRecord, Profile]("profile")
 val userMeta = JooqMeta.metaOf[tables.FullUser, FullUserRecord, FullUser]
```
Where the **namespacedMetaOf** does the magic
But if this seams to be too match work, we can simple do:
```
 val userMeta = JooqMeta.metaOf[tables.FullUser, FullUserRecord, FullUser]
```
And with only that, all the other macros will be auto generated.

###Base DAO functionality
Now, it could be nice to have the most basic functionalities already implemented. For that there is a DAO with the most necessary methods. The usage is as simple as:
```
  implicit lazy val userMeta = JooqMeta.metaOf[tables.User, UserRecord, User]

  lazy val userDAO = new DefaultJooqDAO[UserRecord, User]() {}
```
And with only that, you'll have this operations already implemented:
```
findAll(): List[User]
insert(user: User): Long
insert(user: User, more: User*): Unit
insertAll(users: Seq[User]): Unit
update(user: User): Int
update(user: User, more: User*): Int
updateAll(users: Seq[User]): Int
delete(user: User): Int
delete(user: User, more: User*): Int
deleteAll(users: Seq[User]): Int
deleteAll(): Int
deleteById(id: Long): Int
deleteById(id: Long, more: Long*): Int
deleteByIds(ids: Seq[Long]): Int
findById(id: Long): Option[User]
findByIds(id: Long, more: Long*): List[User]
findByIds(ids: Seq[Long]): List[User]
```
This dao implementation, works for entities with a single **primaryKey**, with type **Long** or **Int** and **autoincremental**.
In case of, for example, a **String** **primaryKey**, can be resolved with:

```
  implicit lazy val userMeta = JooqMeta.metaOf[tables.UserByCode, UserByCodeRecord, UserByCode]

  lazy val userDAO = new JooqDAO[UserByCodeRecord, String, UserByCode]() {}
```
Where the second type parameter in the **JooqDAO** indicates that this table has a **String** as a **primaryKey**. <br />
Another use case, is that the **User.id** is not autogenerated, for example an **UUID**. In that case, this can resolved simply doing
```
  implicit lazy val userMeta = JooqMeta.metaOf[tables.User, UserRecord, User]

  lazy val userDAO = new DefaultJooqDAO[UserRecord, User]() {
    override lazy val createFields = userMeta.fields
  }
```
This means: During creation, use all the fields of the user. Does not ignore the **primaryKey** like the default implementation. <br />
Another use case, may be enhanced functionality, like a **saveOrUpdate** that depends on the SQL implementation. <br /> This is an extension for **saveOrUpdate** in a **MySQL** store

```
  class MySQLDefaultDAO [Rec <: UpdatableRecord[Rec], Entity] extends DefaultJooqDAO[Rec, Entity] {
    def saveOrUpdate(e: Entity)(implicit dsl: DSLContext): Unit = {
      val createRecord = attached(e)
      val updateRecord = attached(e).into(updateFields :_*)
      dsl.insertInto(table).set(createRecord).onDuplicateKeyUpdate().set(updateRecord).execute()
    }

    def saveOrUpdate(e: Seq[Entity])(implicit dsl: DSLContext): Unit = e.foreach(saveOrUpdate)

    def saveOrUpdate(e: Entity, en: Entity*)(implicit dsl: DSLContext): Unit = saveOrUpdate(en :+ e)
  }
```
Now all the DAOs that extends **MySQLDefaultDAO** instead of **DefaultJooqDAO** will have the **saveOrUpdate** functionality. <br />
**Important Note:** <br />
All the methods definitions in the default dao, actually receives one more implicit parameter. That parameter is the **Jooq DSLContext** used for the dao operations. So the whole usage example should be:
```
    val configuration = new DefaultConfiguration()
      ...
    implicit val dsl = DSL using configuration
      ...
    userDAO insert User(0l, "name", "last")
```
###Base query generator
This feature is simple in the beginning, but combined with the one-to-one, one-to-many, etc support, may be very powerful. <br />
With this, a query maybe autogenerated. So, for example:
```
  val userMeta = JooqMeta.metaOf[tables.User, UserRecord, User]
  println(userMeta.query)
```
Generates this code:
```
select 
  "PUBLIC"."USER"."ID", 
  "PUBLIC"."USER"."FIRST_NAME", 
  "PUBLIC"."USER"."LAST_NAME"
from "PUBLIC"."USER"
```
**NOTE:** This will autogenerate the joins for the x-to-x support
###One to one / one to many support
With this feature, it's possible to fetch eagerly a joined entity. <br />
For example, giving this schema:
```
CREATE TABLE `city` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(50),
  `state_id` bigint(20),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `state` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(50),
  `country_id` bigint(20),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `country` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(50),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```
This can be mapped with this entities and mappers
```
case class City(id: Long, name: String, state: State)

case class State(id: Long, name: String, country: Country)

case class Country(id: Long, name: String)

...

        implicit lazy val countryMeta = JooqMeta.metaOf[tables.Country, CountryRecord, Country]
        implicit lazy val stateMeta = JooqMeta.metaOf[tables.State, StateRecord, State]
        implicit lazy val cityMeta = JooqMeta.metaOf[tables.City, CityRecord, City]
```
This **JooqMetas** may be used with the **JooqDAOs**, or simply doing:
```
   println(cityMeta.query)
```
will be autogenerated the following query
```
select 
  "PUBLIC"."CITY"."ID", 
  "PUBLIC"."CITY"."NAME", 
  "PUBLIC"."STATE"."ID", 
  "PUBLIC"."STATE"."NAME", 
  "PUBLIC"."COUNTRY"."ID", 
  "PUBLIC"."COUNTRY"."NAME", 
  "PUBLIC"."STATE"."COUNTRY_ID", 
  "PUBLIC"."CITY"."STATE_ID"
from "PUBLIC"."CITY"
  join "PUBLIC"."STATE"
  on "PUBLIC"."CITY"."STATE_ID" = "PUBLIC"."STATE"."ID"
  join "PUBLIC"."COUNTRY"
  on "PUBLIC"."STATE"."COUNTRY_ID" = "PUBLIC"."COUNTRY"."ID"
```
**IMPORTANT:** Only the fetching is done eagerly, but there is no cascade functionality.

###Next functionalities
--------

 1. Custom join field
 2. Many to many functionality
 3. Basic query generator

###Limitations
------------

 1. Does not support a two way relation. Something like this:
```
case class Profile(firstName: String, lastName: String)

case class User(id: Long, profile: Profile)
```
In this case, the **User** knowns the **Profile**. And the **Profile** knowns the **User**. So the macro can never be resolved.
 

###Contributing
------------
Please see [CONTRIBUTING](CONTRIBUTING.md)
