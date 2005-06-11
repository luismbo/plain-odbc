;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; Foreign function compatibility module for MCL, CMUCL, LispWorks, ACL
;; and CormanLisp (CMUCL version)
;; Version 0.9
;; Copyright (C) Paul Meurer 2000, 2001. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

(defpackage "FFC"
  (:use "COMMON-LISP" "ALIEN" "C-CALL" "SYSTEM")
  (:export
   "*FOREIGN-MODULE*" "DEFINE-FOREIGN-FUNCTION" 
   "MAKE-RECORD"
   "%WITH-TEMPORARY-ALLOCATION" "%WITH-SQL-POINTER" "%GET-CSTRING"
   "%CSTRING-INTO-STRING"
   "%CSTRING-INTO-VECTOR"
   "%GET-CSTRING-LENGTH" "WITH-CSTR" "%GET-PTR" "%NEW-PTR" "%DISPOSE-PTR"
   "%GET-SIGNED-WORD"
   "%GET-UNSIGNED-LONG"
   "%GET-SIGNED-LONG"
   "%GET-SINGLE-FLOAT"
   "%GET-DOUBLE-FLOAT"
   "%GET-WORD"
   "%GET-LONG"
   "%GET-SHORT"
   "%GET-SIGNED-LONG"
   "%GET-BIT"
   "%PUT-STR"
   "%PUT-WORD"
   "%PUT-SHORT"
   "%PUT-LONG"
   "%NEW-CSTRING" 
   "%NULL-PTR"
   "%PTR-EQL"
   "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"
   "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
   "DEFINE-FOREIGN-TYPE"
   "FOREIGN-SLOT"))

