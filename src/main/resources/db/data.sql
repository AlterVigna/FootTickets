INSERT IGNORE INTO footticket.account VALUES ("user","pass",500,"B");
INSERT IGNORE INTO footticket.account VALUES ("admin","pass",500,"A");
INSERT IGNORE INTO footticket.account VALUES ("alice","pass",1000,"B");
INSERT IGNORE INTO footticket.account VALUES ("bob","pass",1000,"B");


INSERT IGNORE INTO footticket.transaction (`id`,`numSeats`,`location`, `price`,`username`) VALUES (1,3,'A1-A2-A3','150.00','user');
INSERT IGNORE INTO footticket.transaction (`id`,`numSeats`,`location`, `price`,`username`) VALUES (2,2,'B1-B2','100.00','user');