;;; -*- Mode: lisp -*-

(defpackage :test-mysql
  (:use :common-lisp :plain-odbc))

(export 'run-all-tests)

(in-package :test-mysql)

(defun run-all-tests (con)
  (make-mysql-type-test con))

(defparameter *mysql-type_test-ddl* "  

create table type_test
(id int,
t_TINYINT TINYINT,
t_SMALLINT  SMALLINT,
t_MEDIUMINT mediumint,
t_INT int,
t_BIGINT bigint,
t_float24 float(24),
t_float53 float(53),
t_float float,
t_DOUBLE double,
t_decimal40_20 DECIMAL(40,20) ,

t_dATE date,

t_DATETIME datetime,
t_TIMESTAMP timestamp,
t_TIME 	time,
t_YEAR 	year,
T_char char(255),
t_varchar varchar(255),

t_TINYBLOB tinyblob, 
t_TINYTEXT tinytext,
t_BLOB blob,
t_TEXT text,
t_MEDIUMBLOB mediumblob, 
t_MEDIUMTEXT mediumtext,
t_LONGBLOB longblob, 
t_LONGTEXT longtext

)
")
(defun make-mysql-type-test (con)
  (ignore-errors (exec-command con "drop table type_test"))
  (exec-command con *mysql-type_test-ddl*)
  (exec-update con "insert into type_test (id) values(1)")
  (exec-update con "
  update type_test set
    t_tinyint =1,
    t_smallint =255,
   t_MEDIUMINT =256*256*127,
  t_INT =987,
  t_BIGINT =256*256*256*256 *256*256*256*127,
  t_float24 =9.67,
  t_float53 = 1.5/3.9,
  t_float =1.0/7.0,
  t_DOUBLE = 1.0/7.0,
  t_decimal40_20 = '12345678901234567890.1234567890123456789',
  t_dATE ='2004-6-25',
  t_DATETIME = '2004-5-13 13:56:34',
  /* t_TIMESTAMP timestamp,  */
  t_TIME 	= '12:56',
  t_YEAR 	=1967,
  T_char = 'abcdefghijkmlnop',
  t_varchar = 'abcdefghijklmnopqrstuvw',
/*  --  t_TINYBLOB = lpad('a',33000), 
  --  t_TINYTEXT lpad('a',33000),
  --  t_BLOB blob,
  --  t_TEXT text,
  --  t_MEDIUMBLOB mediumblob, 
  --  t_MEDIUMTEXT mediumtext, 
  t_LONGBLOB longblob, */
  t_LONGTEXT = lpad('a',33000,'x') 
  where id =1
")
  (commit con)
  (let ((res (exec-query con "
    select t_tinyint,
           t_smallint,
           t_mediumint,
           t_int,
           t_bigint,
           t_float24,
           t_float53,
           t_float,
           t_double,
           t_decimal40_20,
           t_date,
           t_datetime,
           t_time,
           t_year,
           t_char,
           t_varchar,
           t_longtext
        from type_test where id=1
")))
    (pprint res))
  (let ((stm (prepare-statement con "update type_test set t_longblob =?, t_longtext=? where id =1" '((:blob :in) (:clob :in)))))
    (exec-prepared-update stm 
                        (List 
                         (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 33)
                         (make-string 100001 :initial-element #\o))))
  (commit con))





