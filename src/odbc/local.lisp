;;;-*- Mode: Lisp; Package: FFC -*-

;********************************************************
; file:        local.lisp                                
; description: Values for local installation.            
; date:        Thu Jun  7 2001 - 16:26                   
; author:      Liam Healy <Liam.Healy@nrl.navy.mil>      
; modified:    Thu Jun  7 2001 - 16:27
;********************************************************

(in-package :ffc)

;; adapt the library location to your needs

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+:mcl
  (setf *foreign-module* "vsi:ODBC$DriverMgr")
  
  #+(and :allegro (not :unix))
  (setf *foreign-module* "odbc32.dll")
  
  #+(and :lispworks :win32)
  (setf *foreign-module* "odbc32")
  
  #+(and :unix (not :lispworks))
  (setf *foreign-module* "/usr/local/lib/libodbc.so")
  
  #+(and :lispworks :unix)
  (setf *foreign-module* "/usr/openlink/lib/oplodbc.so.1"))
