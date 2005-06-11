;;; -*- Mode: lisp  -*-



(defpackage :test-oracle
  (:use :common-lisp :plain-odbc)
  )

(in-package :test-oracle)

(export 'run-all-tests)


(defun run-all-tests (con)
  (oracle-type-test con)
  (test1 con)
  (test2 con)
  (test3 con)
  (test4 con)
  (test5 con)
  (test6 con))
  
 
; this function replaces in a string every (code-char 13)  
; (code-char 10)by #\space
; this is needed for oracle PL/SQL statements
(defun fix13 (str)
  (substitute #\space (code-char 10) (substitute #\space (code-char 13) str)))

       

(defparameter *oracle-type_test-ddl* "
create table type_test (
  id integer,
  t_integer integer,
  t_number number,
  t_char char(2000) ,
  t_varchar varchar(4000),
  t_date date,
  t_raw raw(2000),
  t_blob blob,
  t_clob clob)
")

(defun oracle-type-test (con)
  (if (not (zerop (caar (exec-query con "select count(*) from user_tables where table_name ='TYPE_TEST'")))) 
    (exec-command con "drop table type_test"))
  (exec-command con *oracle-type_test-ddl*)
  (exec-update con "insert into type_test (id) values(1)")
  (exec-update con "
   update type_test set
      t_integer= 123456789012345677989,
      t_number = 1.0/3.0,
      t_char=rpad('1',1999),
      t_varchar =lpad('1',3999),
      t_date = sysdate,
      t_raw =hextoraw('11223344556677889900')
    where id =1")
  (exec-query con "select * from type_test")
  (let ((stm (prepare-statement con "update type_test set t_blob=?,t_clob=? where id =1" '((:blob :in) (:clob :in)))))
  (exec-prepared-update stm 
                        (List 
                         (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 33)
                         (make-string 100001 :initial-element #\o))))
  (commit con))

 

(defun drop-test-proc (con proc)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from user_objects where object_name='~A'" proc))))
    (exec-command con (format nil "drop procedure ~A" proc))))


(defun test1 (con)
  (drop-test-proc con "TEST99")
  (exec-command con (fix13 "
  create procedure TEST99 (a integer,b out integer) as 
  begin 
    b:=a+1; 
  end;
 "))
  (commit con)
  (let ((stm (prepare-statement con "{call TEST99(?,?)}" 
                                '((:integer :in) 
                                  (:integer :out)))))
    (assert (= 2 (first (exec-prepared-command stm (list 1)))))
    (free-statement stm)))


(defun test2 (con)
  (drop-test-proc con "TEST99")
  (exec-command con (fix13 "
  create procedure TEST99 (a varchar2,b out varchar2) as
  begin
    b:=a;
  end;
 "))
  (commit con)
  (let ((stm (prepare-statement con "{call TEST99(?,?)}" 
                                '((:string :in) 
                                  (:string :out)))))
    (let ((str "lölkälkäölkälhjajhgfsjgakjhgfjfjhgffdtrtreztr"))
      (assert (equal str (first (exec-prepared-command stm (list str)))))
      (free-statement stm)))
  (commit con))



(defun test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (drop-test-proc con "TEST99")
    (let ((a (caar (exec-query con "select sysdate from dual"))))
      (exec-command con (fix13 "
  create procedure TEST99 (a date,b out date ) as 
  begin
    b:=a+1; 
  end;
  "))  
      (with-prepared-statement 
          (stm con "{call TEST99(?,?)}" 
               '((:date :in) (:date :out)))
        (let ((res (exec-prepared-command stm (list "3323283742"))))
          (assert (equal res (list (write-to-string (+ 3323283742 86400))))))))
    (commit con)))


(defun test4 (con)
  (drop-test-proc con "TEST99")
  (exec-command con (fix13 "
   create procedure TEST99 (a in out varchar2, b in out varchar2) as
    x varchar2(1000); begin x:=a;a:=b;b:=x; end;"))
  (with-prepared-statement (stm con "{call TEST99(?,?)}" 
                                '((:string :inout) (:string :inout)))
    (let ((res (exec-prepared-command stm (list "abc" "xyz"))))
      (assert (equal res (list "xyz" "abc"))))))
 

(defun test5 (con)
  (drop-test-proc con "TEST99")
  (exec-command con (fix13 "
   create procedure TEST99 (a raw,b out raw) as 
   begin 
     b:=a;
   end;
   "))
  (with-prepared-statement (stm con "{call TEST99(?,?)}" 
                                '((:binary :in) (:binary :out)))
    (let* ((guid (caar (exec-query con "select sys_guid() from dual")))
           (res (exec-prepared-command stm (list guid))))
      (assert (equalp guid (first res))))
    (commit con)))



(defun universal-time-list (time)
  (reverse (subseq (multiple-value-list (decode-universal-time time)) 0 6 )))

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

    (drop-test-proc con "TEST99")
    (exec-command con (fix13 "
     create procedure TEST99 (a date, b out date) as 
    begin
     b:=a+2;
    end;
    "))
    (with-prepared-statement (stm con "{call TEST99(?,?)}" '(:date (:date :out)))
      (let ((res (exec-prepared-command stm '((2003 3 4)))))
        (assert (equal res '((2003 3 6 0 0 0))))))
    (let ((res (exec-query con "
      select to_date('8.6.2005','dd.mm.yyyy') -1.0 / (86400-1) from dual")))
      (assert (equal res '(((2005 6 7 23 59 59))))))))


(defun test7 (con)
  ())