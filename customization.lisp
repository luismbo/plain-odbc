;;; -*- Mode: lisp -*-

(in-package :common-lisp-user)

; the filename of the odbc shared library, 
; should be fixed for windows, 
; but for unix I do not know
 

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *odbc-library-file* 
    #+(and clisp unix) "libodbc.so"
    #+(and clisp win32) "odbc32.dll"
    #+(and (or allegro ALLEGRO-CL-TRIAL) MICROSOFT-32)  "odbc32.dll"  )
  (export '*odbc-library-file*))
