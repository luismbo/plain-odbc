;;; -*- Mode: lisp  -*-


(in-package :test-plain-odbc)

(export '(run-oracle-tests))

(defun run-oracle-tests (con)
  (oracle-type-test con)
  (ora-test1 con)
  (ora-test2 con)
  (ora-test3 con)
  (ora-test4 con)
  (ora-test5 con)
  (ora-test6 con)
  (ora-test7 con)
  #+ignore  ;; unicode support does not work for oracle
  (ora-test8 con)
  (ora-test9 con)
  (ora-test10 con)
  (ora-test11 con)
  )
  
 
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

 

(defun ora-drop-test-proc (con proc)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from user_objects where object_name='~A'" proc))))
    (exec-command con (format nil "drop procedure ~A" proc))))


(defun ora-test1 (con)
  (ora-drop-test-proc con "TEST99")
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


(defun ora-test2 (con)
  (ora-drop-test-proc con "TEST99")
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



(defun ora-test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (ora-drop-test-proc con "TEST99")
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


(defun ora-test4 (con)
  (ora-drop-test-proc con "TEST99")
  (exec-command con (fix13 "
   create procedure TEST99 (a in out varchar2, b in out varchar2) as
    x varchar2(1000); begin x:=a;a:=b;b:=x; end;"))
  (with-prepared-statement (stm con "{call TEST99(?,?)}" 
                                '((:string :inout) (:string :inout)))
    (let ((res (exec-prepared-command stm (list "abc" "xyz"))))
      (assert (equal res (list "xyz" "abc"))))))
 

(defun ora-test5 (con)
  (ora-drop-test-proc con "TEST99")
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



(defun ora-test6 (con)
  (let ((*universal-time-to-date-dataype* 'universal-time-list)
        (*date-datatype-to-universal-time* 'list-universal-time))

    (ora-drop-test-proc con "TEST99")
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


(defun ora-test7 (con)
  (let ((filename (namestring (merge-pathnames "odb-trace-test.log" *test-temp-dir*))))
    (when (probe-file filename)
      (DELETE-FILE filename))
    (assert (not (probe-file filename)))
    (trace-connection con filename)
    (dotimes (i 5) (exec-query con "select * from dual"))
    (with-open-file (f filename :direction :input)
      (assert (> (file-length f) 500)))
    (untrace-connection con)
    ;(break)
    (DELETE-FILE filename)
    (exec-query con "select * from dual")
    (assert (not (probe-file filename)))
    ))

;; works only with oracle 9 ?
;; this does not work. mybe with the oracle odbc driver?
(defun ora-test8 (con)
  (ignore-errors (exec-command con "drop table testtab99"))
  (exec-command con "create table testtab99 (id integer, txt nvarchar2(2000))")
  (let ((str (coerce (list #\a #\j (code-char 1000) (code-char 2000) #\o) 'string)))
  (with-prepared-statement (stm con "insert into testtab99 (id,txt) values(?,?)"
                                '((:integer :in) (:unicode-string :in)))
    (exec-prepared-update stm (list 1 str)))
  (let ((res (exec-query con "select txt from testtab99 where id =1")))
    (assert (equal (list str) res)))))


(defun ora-test9(con)
  (let ((res (exec-query con "select to_date('2005-6-7 13:04:45','yyyy-mm-dd hh24:mi:ss' ) as a from dual")))
    (assert (= (encode-universal-time 45 4 13 7 6 2005) (caar res)))))

(defun ora-test10(con)
  (with-prepared-statement (stm con "
          select to_char(?,'yyyy-mm-dd hh24:mi:ss')
          from dual" 
                                '((:date :in)))
    (let ((res (exec-prepared-query stm (list 
                                         (encode-universal-time 1 2 3 13 10 2005)))))
      (assert (equalp "2005-10-13 03:02:01" (caar res))))))

(defun ora-test11(con)
  (exec-command con (fix13 "
     create or replace package test99_pkg as
       type refcursor is ref cursor;
       procedure test_cursor(v varchar2,c in out refcursor);
     end;"))
  (exec-command con (fix13 "
     create or replace package body test99_pkg as
      procedure test_cursor(v varchar2,c in out refcursor) is
      begin
        open c for select v as a,'1234567890' as b from dual;
      end;
     end;"))
  (with-prepared-statement (stm con 
                                "{call test99_pkg.test_cursor(?,?)}" 
                                '((:string :in )))
    (let ((str "just a string"))
      (let ((res (exec-prepared-query stm (list str))))
        (assert (equal res (list (list str "1234567890"))))))))

       

