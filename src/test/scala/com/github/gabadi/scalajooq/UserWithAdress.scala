package com.github.gabadi.scalajooq


case class Address(street: String, number: Long, telephone: String)

case class UserWithAddress(id: java.lang.Long, firstName: String, lastName: String, address: Address)

case class UserWithOptAddress(id: java.lang.Long, firstName: String, lastName: String, address: Option[Address])

case class Name(first: String, last: String)

case class Profile(name: Name)

case class Location(address: Address)

case class ProfileUser(id: Long, profile: Profile)

case class ProfileUserOpt(id: Long, profile: Option[Profile])

case class FullUser(id: Long, profile: Profile, home: Location, work: Location)

case class FullUserOpt(id: Long, profile: Option[Profile], home: Option[Location], work: Option[Location])