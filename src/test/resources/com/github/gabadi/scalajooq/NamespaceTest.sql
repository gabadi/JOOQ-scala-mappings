CREATE TABLE `user_with_address` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `first_name` varchar(50) NOT NULL,
  `last_name` varchar(50) NOT NULL,
  `address_street` varchar(50),
  `address_telephone` varchar(50),
  `address_number` bigint(20),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `full_user` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `profile_name_first` varchar(50),
  `profile_name_last` varchar(50),
  `home_address_street` varchar(50),
  `home_address_telephone` varchar(50),
  `home_address_number` bigint(20),
  `work_address_street` varchar(50),
  `work_address_telephone` varchar(50),
  `work_address_number` bigint(20),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
