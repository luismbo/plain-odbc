;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :common-lisp-user) 

(defpackage "PLAIN-ODBC"
  (:use 
   "COMMON-LISP" #+mcl "CCL" #+cormanlisp "WIN32" "FFC")
  (:export
   "EXEC-QUERY" 
   "EXEC-UPDATE" 
   "EXEC-COMMAND"
   "PREPARE-STATEMENT" 
   "EXEC-PREPARED-QUERY" 
   "EXEC-PREPARED-UPDATE"
   "EXEC-PREPARED-COMMAND"
   "FREE-STATEMENT"
   "CONNECT"
   ;"DRIVER-CONNECT"
   "CONNECT-GENERIC"
   "CLOSE-CONNECTION"
   "COMMIT"
   "ROLLBACK"
   
   "WITH-PREPARED-STATEMENT"
   
   "*UNIVERSAL-TIME-TO-DATE-DATAYPE*"
   "*DATE-DATATYPE-TO-UNIVERSAL-TIME*"

   ;; utilities
   "*DEFAULT-ACCESS-DSN*"
   "*DEFAULT-ORACLE-DSN*"
   "*DEFAULT-SQL-SERVER-DSN*"
   
   "CONNECT-ACCESS"
   "CONNECT-SQL-SERVER"
   "CONNECT-ORACLE" 

   "TRACE-CONNECTION"
   "UNTRACE-CONNECTION"
)) 
 
