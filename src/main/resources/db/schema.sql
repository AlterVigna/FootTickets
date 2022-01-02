CREATE TABLE IF NOT EXISTS ACCOUNT (
                         `Username` VARCHAR(50)  NOT NULL,
                         `Password` VARCHAR(50)  NOT NULL,
                         `amount` DECIMAL(11,2) NOT NULL DEFAULT 0,
                         `Type` VARCHAR(1) NOT NULL DEFAULT 'U',
                         PRIMARY KEY (`Username`)
);