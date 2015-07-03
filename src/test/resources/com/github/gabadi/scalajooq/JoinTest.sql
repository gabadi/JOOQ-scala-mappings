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

CREATE TABLE `city_code` (
  `code` varchar(50),
  `name` varchar(50),
  `state_code` varchar(50),
  PRIMARY KEY (`code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `state_code` (
  `code` varchar(50),
  `name` varchar(50),
  PRIMARY KEY (`code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

