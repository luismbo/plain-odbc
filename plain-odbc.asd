;;; -*- Mode: Lisp -*-


;;; 
#+clisp
(asdf:defsystem "uffi"
    :pathname "./src/uffi-clisp/"
    :components ((:file "uffi"))) 



(asdf:defsystem "plain-odbc"
  ;:package :cl-user
  ;:binary-pathname (translate-logical-pathname "plain-odbc:bin;")
  
  ;:source-extension "lisp"
  :components (
               (:module "odbc-stuff"
                        :pathname "src/odbc/"
                        :components 
                        ((:file "plain-odbc-package")
                         (:file "odbc-constants")
                         (:file "global")
                         (:file "uffi-support")
                         (:file "odbc-ff-interface")
                         (:file "odbc-functions")
                         (:file "parameter")
                         (:file "column")
                         (:file "odbc-main")
                         (:file "odbc-utilities"))
                        :serial t
                        ))
  :serial t
  :depends-on (:uffi))