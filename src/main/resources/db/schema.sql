CREATE TABLE IF NOT EXISTS ACCOUNT (
                         `Username` VARCHAR(50)  NOT NULL,
                         `Password` VARCHAR(50)  NOT NULL,
                         `amount` DECIMAL(11,2) NOT NULL DEFAULT 0,
                         `Type` VARCHAR(1) NOT NULL DEFAULT 'U',
                         PRIMARY KEY (`Username`)
);

CREATE TABLE IF NOT EXISTS TRANSACTION (
                             `id` INTEGER  NOT NULL AUTO_INCREMENT,
                             `numSeats` INTEGER  NOT NULL DEFAULT 0,
                             `location` VARCHAR(200) NOT NULL DEFAULT '',
                             `price` DECIMAL(11,2) NOT NULL DEFAULT 0,
                             `timestamp` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                             `username` VARCHAR(50) NOT NULL,
                             PRIMARY KEY (`id`),
                             constraint `username` FOREIGN KEY (`username`)
                                REFERENCES account(`username`)
                                ON DELETE NO ACTION ON UPDATE CASCADE
);