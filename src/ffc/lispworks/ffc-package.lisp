;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; Foreign function compatibility module for MCL, CMUCL, LispWorks, ACL 
;; and CormanLisp (LW version)
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

;; This file contains LW specific code.

(defpackage "FFC"
  (:use "COMMON-LISP")
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
    "%GET-LONG"
    "%GET-SHORT"
    "%GET-SIGNED-LONG"
    "%GET-BIT"
    "%GET-NTH-BYTE"
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"
    "%NEW-CSTRING" 
    "%NULL-PTR"
    "%PTR-EQL"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
    "%GET-STRING"
    "%NEW-ARRAY"
    "DEFINE-FOREIGN-TYPE"
    "FOREIGN-SLOT"))
