;;; -*- Mode:lisp -*-

(defpackage :test-sql-server 
  (:use :common-lisp :plain-odbc)
  )

(in-package :test-sql-server)

(export 'run-all-tests)


(defun run-all-tests (con)
  (sql-server-type-test con)
  (test1 con)
  (test2 con)
  (test3 con)
  (test4 con)
  (test5 con)
  (test6 con))


(defparameter *sql-server-type_test-ddl* "
CREATE TABLE [type_test] (
	[id] [int] NOT NULL ,
	[t_bigint] [bigint] NULL ,
	[t_binary] [binary] (50) NULL ,
	[t_bit] [bit] NULL ,
	[t_char] [char] (10) NULL ,
	[t_datetime] [datetime] NULL ,
	[t_decimal] [decimal](38, 9) NULL ,
	[t_float] [float] NULL ,
	[t_image] [image] NULL ,
	[t_money] [money] NULL ,
	[t_nchar] [nchar] (10)  NULL ,
	[t_ntext] [ntext]  NULL ,
	[t_numeric] [numeric](38, 19) NULL ,
	[t_nvarchar] [nvarchar] (2000)  NULL ,
	[t_varchar] [varchar] (2000)  NULL ,
	[t_real] [real] NULL ,
	[t_smalldatetime] [smalldatetime] NULL ,
	[t_smallint] [smallint] NULL ,
	[t_smallmoney] [smallmoney] NULL ,
--	[t_sql_variant] [sql_variant] NULL ,
	[t_text] [text]  NULL ,
	[t_timestamp] [timestamp] NULL ,
	[t_tinyint] [tinyint] NULL ,
	[t_uniqueidentifier] [uniqueidentifier] NULL ,
	[t_varbinary] [varbinary] (50) NULL ,
	CONSTRAINT [PK_type_test] PRIMARY KEY  CLUSTERED 
	(
		[id]
	)  
)
")

(defun sql-server-type-test (con)
  (if (/= 0 (caar (exec-query con "select count(1) from sysobjects where name = 'type_test'")))
    (exec-command con "drop table type_test"))
  (exec-command con  *sql-server-type_test-ddl*)
  (exec-command con "insert into type_test (id) values(1)")
  (exec-update con "
  update type_test set     
    t_bigint=256*256*256*127,
    t_binary = convert(binary(50), newid()),
    t_bit = 1,
    t_char ='1234567890',
    t_datetime = getdate(),
    t_decimal  = 123456789.123456789,
    t_float = 1.0/3.0,
    t_money = 1.0/.70,
    t_nchar = '12356788',
    t_numeric = convert(numeric(38,19),1)/convert(numeric(38,19),11),
    t_nvarchar = replicate('abcdefghi',200),
    t_real = 1.0/99.0,
    t_smalldatetime = '2004-6-7 12:58:59',
    t_smallint = 12,
	  t_smallmoney = 5678,
	  --t_sql_variant= 'a',
	 --[t_text] [text]  NULL ,
	 --[t_timestamp] [timestamp] NULL ,
	  t_tinyint =1,
	  t_uniqueidentifier = newid(),
	  t_varbinary = convert(varbinary(10), 'aabbccddeeff11223344')
   where id = 1
"
               )
  (commit con)
  (exec-query con "
   select t_bigint,
          t_binary,
          t_bit,
          t_char,
          t_datetime,
          t_float,
          t_money,
          t_nchar,
          t_numeric,
          t_nvarchar,
          t_real,
          t_smalldatetime,
          t_smallint,
          t_smallmoney,
          t_tinyint,
          t_uniqueidentifier,
          t_varbinary 
    from type_test where id =1
")
  (let ((stm (prepare-statement con "
   update type_test set t_image =?,t_text=?,t_ntext=? where id  = 1"
                                '((:blob :in) (:clob :in) (:clob :in)))))
    (exec-prepared-update stm 
                          (list 
                           (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 33)
                           (make-string 100001 :initial-element #\o)
                           (make-string 100001 :initial-element #\o)
                           )))
  (commit con))

(defun drop-test-proc (con proc)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from sysobjects where name='~A'" proc))))
    (exec-command con (format nil "drop procedure ~A" proc))))

(defun test1 (con)
  (drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a int,@b int out
  as
   set @b=@a+1
 ")
  (commit con)
  (let ((stm (prepare-statement con "{call test99(?,?)}" 
                                '((:integer :in) 
                                  (:integer :out)))))
    (assert (= 2 (first (exec-prepared-command stm (list 1)))))
    (free-statement stm)))


(defun test2 (con)
  (drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a varchar(200),@b varchar(200) out
  as
   set @b=@a
 ")
  (commit con)
  (let ((stm (prepare-statement con "{call test99(?,?)}" 
                                '((:string :in) 
                                  (:string :out)))))
    (let ((str "lölkälkäölkälhjajhgfsjgakjhgfjfjhgffdtrtreztr"))
      (assert (equal str (first (exec-prepared-command stm (list str)))))
      (free-statement stm)))
  (commit con))

(defun test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (drop-test-proc con "test99")
    (let ((a (caar (exec-query con "select getdate()"))))
      (exec-command con "create procedure 
    test99 @a datetime,@b datetime out as 
    set @b= dateadd(d,1,@a);return 789 ")
      (with-prepared-statement 
          (stm con "{?=call test99(?,?)}" 
               '((:integer :out) (:date :in) (:date :out)))
        (let ((res (exec-prepared-command stm (list "3323283742"))))
          (assert (equal res (list 789 (write-to-string (+ 3323283742 86400))))))))
    (commit con)))

(defun test4 (con)
  (drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a varchar(1000) out ,@b varchar(1000) out as
    declare @x varchar(1000); set @x=@a;set @a=@b; set @b=@x ;return 99")
  (with-prepared-statement (stm con "{?=call test99(?,?)}" 
                                '((:integer :out) (:string :inout) (:string :inout)))
    (let ((res (exec-prepared-command stm (list "abc" "xyz"))))
      (assert (equal res (list 99 "xyz" "abc"))))))
 


(defun test5 (con)
  (drop-test-proc con "test99")
  (exec-command con "create procedure test99 
    @a uniqueidentifier ,@b uniqueidentifier out as set @b=@a;")
  (with-prepared-statement (stm con "{call test99(?,?)}" 
                                '((:binary :in) (:binary :out)))
    (let* ((guid (caar (exec-query con "select newid()")))
           (res (exec-prepared-command stm (list guid))))
      (assert (equalp guid (first res))))
    (commit con)))



(defun universal-time-list (time)
  (reverse (subseq (multiple-value-list (decode-universal-time time)) 0 6  )))

(defun list-universal-time (list)
  (assert list ())
  (encode-universal-time         
   (or (sixth list) 0)
   (or (fifth list) 0)
   (or (fourth list) 0)
   (or (third list) 1)
   (or (second list) 1)
   (or (first list) 1900)))

(defun test6 (con)
  (let ((*universal-time-to-date-dataype* 'universal-time-list)
        (*date-datatype-to-universal-time* 'list-universal-time))

    (drop-test-proc con "test99")
    (exec-command con "create procedure test99
   @a datetime, @b datetime out as set @b=dateadd(d,2,@a)")
    (with-prepared-statement (stm con "{call test99(?,?)}" '(:date (:date :out)))
      (let ((res (exec-prepared-command stm '((2003 3 4)))))
        (assert (equal res '((2003 3 6 0 0 0))))))
    (let ((res (exec-query con "select dateadd(s,86399,'2005-6-7')")))
      (assert (equal res '(((2005 6 7 23 59 59))))))))
  




