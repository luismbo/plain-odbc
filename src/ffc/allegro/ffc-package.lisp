;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; Foreign function compatibility module for MCL, LW and ACL (ACL version)
;; Version 0.9
;; Copyright (C) Paul Meurer 1999, 2000. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; In this file the platform specific code is isolated.
;; The code in this file consists mostly of wrapper functions and macros 
;; around the platform-dependent foreign function interface.

;; This file contains Allegro Common Lisp (Version 5.0) specific code

(in-package :cl-user)

(defpackage "FFC"
  (:use "COMMON-LISP")
  (:export 
;"*FOREIGN-MODULE*" 
"DEFINE-FOREIGN-FUNCTION" 
    "MAKE-RECORD"
    "%WITH-TEMPORARY-ALLOCATION" "%WITH-SQL-POINTER"
    "%GET-CSTRING"
    "%GET-STRING"
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
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"

    "%PUT-SINGLE-FLOAT"
    "%PUT-DOUBLE-FLOAT"

    "%NEW-CSTRING" 
    "%NULL-PTR"
    "%PTR-EQL"
    "SHORT-TO-SIGNED-SHORT" ; #+allegro
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"
    "%NEW-ARRAY"
    "%GET-NTH-BYTE"
    "DEFINE-FOREIGN-TYPE"
    "FOREIGN-SLOT"))

