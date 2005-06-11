;;;; By adding the following lines in the Corman Lisp init.lisp
;;;; startup file you can use the following to automatically load
;;;; and compile the SQL-ODBC packages:
;;;;
;;;; (require 'ODBC)
;;;;

;; This should be the directory pointing to the 
;; sql-odbc installation.
(defconstant *sql-odbc-directory* "d:\\programs\\ccl\\sql\\")

(defun sql-odbc-directory-name (relative-name)
	(concatenate 'string *sql-odbc-directory* relative-name))

(ccl:register-module-source "SQL" 
	(list 
		(sql-odbc-directory-name "corman\\patches.lisp")
		(sql-odbc-directory-name "corman\\clos-eql-patch.lisp")
		(sql-odbc-directory-name "corman\\class-slot-patch.lisp")
		(sql-odbc-directory-name "sql\\sql-system.lisp")
		(sql-odbc-directory-name "sql\\sql-package.lisp")
		(sql-odbc-directory-name "sql\\sql-class.lisp")
		(sql-odbc-directory-name "sql\\sql-expressions.lisp")
		(sql-odbc-directory-name "sql\\sql-functional-interface.lisp")))

(ccl:register-module-source "ODBC" 
	(list 
		(sql-odbc-directory-name "corman\\ff-compatibility-corman.lisp")
		(sql-odbc-directory-name "odbc\\odbc-system.lisp")
		(sql-odbc-directory-name "odbc\\odbc-package.lisp")
		(sql-odbc-directory-name "odbc\\odbc-constants.lisp")
		(sql-odbc-directory-name "odbc\\odbc-ff-interface.lisp")
		(sql-odbc-directory-name "odbc\\odbc-functions.lisp")
		(sql-odbc-directory-name "odbc\\odbc-sql-interface.lisp")))