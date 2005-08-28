;;; -*- Mode: lisp -*-

(in-package :test-plain-odbc)

(export '(run-mysql-tests))

(defun run-mysql-tests (con)
  (make-mysql-type-test con)
  (mysql-test1 con)
  (mysql-test2 con)
  (mysql-test3 con)
  (mysql-test4 con)
  (mysql-test5 con)
  )

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
    ;(pprint res)
    )
  (let ((stm (prepare-statement 
              con "update type_test set t_longblob =?, t_longtext=? where id =1" 
              '(:blob :in) '(:clob :in))))
    (exec-prepared-update stm 
                          (make-array 10000 :element-type '(unsigned-byte 8) 
                                      :initial-element 33)
                          (make-string 100001 :initial-element #\o)))
(commit con))


(defun mysql-test1 (con)
  (let ((filename (namestring (merge-pathnames "odbc-trace-test.log" *test-temp-dir* ))))
    (when (probe-file filename)
      (DELETE-FILE filename))
    (assert (not (probe-file filename)))
    (trace-connection con filename)
    (dotimes (i 5) (exec-query con "select 1"))
    (with-open-file (f filename :direction :input)
      (assert (> (file-length f) 500)))
    (untrace-connection con)
    ;(break)
    (DELETE-FILE filename)
    (exec-query con "select 1")
    (assert (not (probe-file filename)))))


(defun mysql-test2 (con)
  (let ((str (make-funny-string 245)))
    (exec-update con "delete from type_test where id=99")
    (with-prepared-statement (stm con 
                                  "insert into type_test (id,t_varchar) values (99,?)"
                                  '(:string :in))
      (exec-prepared-update stm str))
  (with-prepared-statement (stm con 
                                "select t_varchar from type_test where id=99" )
    (let ((res (exec-prepared-query stm )))
      (assert (string= (caar res) str))))))


(defun mysql-test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (exec-update con "delete from type_test where id=99")
    (with-prepared-statement (stm con 
                                  "insert into type_test (id,t_datetime) values(99,?)"
                                  '(:date :in))
      (exec-prepared-update stm "3323283742"))
    (let ((res (exec-query con "select  DATE_ADD(t_datetime, INTERVAL 1 DAY) 
                                from type_test where id =99")))
      (assert (equal (parse-integer (caar res)) (+ 86400 3323283742))))))

(defun mysql-test4 (con)
  (exec-update con "delete from type_test where id=99")
  (with-prepared-statement (stm con "insert into type_test (id,t_double) values(99,?)" 
                                '(:double :in))
    (exec-prepared-update stm 1.8))
  (let ((res (exec-query con "select t_double+1 from type_test where id=99")))
    (assert (<= (abs (- (caar res) 2.8d0)) 1d-7))))

                                         

(defun mysql-test5 (con)
  (exec-update con "delete from type_test")
  (commit con)
  (with-prepared-statement (stm con "insert into type_test (id,t_LONGTEXT) values(?,?)" 
                                '(:integer :in) '(:clob :in))
    (let ((mp plain-odbc::*max-precision*))
      (dolist (len (list 0 1 2 3 4 5 900 9000 8192 8000 
                         (1- mp) 
                         mp 
                         (1+ mp)
                         (* 2 mp)
                         (1- (* 2 mp))
                         (1+ (* 2 mp))))
        (let ((string (make-funny-string len)))
          (exec-prepared-update stm len string)
          (let ((res (exec-query con (format nil "select t_longtext from type_test where id=~A" len))))
          (assert (equal res
                         (list (list string)))))))))
    (commit con)
    )
