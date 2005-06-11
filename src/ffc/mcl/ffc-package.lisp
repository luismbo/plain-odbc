;;;-*- Mode: Lisp; Package: FFC -*-

;; Foreign function compatibility module for MCL, CMUCL, LispWorks, 
;; ACL and CormanLisp (MCL version)
;; Version 0.9
;; Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; In this file the platform specific code is isolated.
;; The code in this file consists mostly of wrapper functions and macros 
;; around the platform-dependent foreign function interface.

;; This file contains MCL specific code.

;; TO DO: better efficiency through more declarations

(defpackage "FFC"
  (:use "COMMON-LISP" "CCL")
  (:import-from "CCL" "WITH-CSTR")
  (:export "*FOREIGN-MODULE*" "DEFINE-FOREIGN-FUNCTION" 
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
    "%GET-SHORT"
    "%GET-LONG"
    "%GET-SIGNED-LONG"
    "%GET-BIT"
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"
    "%NEW-CSTRING" 
    "%NULL-PTR"
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"
    "DEFINE-FOREIGN-TYPE"
    "FOREIGN-SLOT"))
