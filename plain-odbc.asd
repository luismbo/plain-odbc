;;; -*- Mode:Lisp -*-


(asdf:defsystem "plain-odbc"
  ;:package :cl-user
  ;:binary-pathname (translate-logical-pathname "plain-odbc:bin;")
 
  ;:source-extension "lisp"
  :components (;; global customization
               (:file "customization")
               ;; this is foreign function compatibility module 
               #+clisp
               (:module "ffc"
                        :pathname "src/ffc/clisp/"   
                        :components ((:file "ffc-package")
                                     (:file "ff-compatibility-clisp"))
                        :serial t)
               #+allegro
               (:module "ffc"
                        :pathname "src/ffc/allegro/"   
                        :components ((:file "ffc-package")
                                     (:file "ff-compatibility-acl"))
                        :serial t)
               ;; the odbc fcuntions
               (:module "odbc-stuff"
                        :pathname "src/odbc/"
                        :components 
                         ((:file "plain-odbc-package")
                          ;(:file "local"        )
                          (:file "odbc-constants")
                          (:file "global")
                          (:file "odbc-ff-interface")
                          (:file "odbc-functions")
                          (:file "parameter")
                          (:file "column")
                          (:file "odbc-main")
                          (:file "odbc-utilities"))
                         :serial t
                         ))
               :serial t) 