;;; -*- Mode: Lisp -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :plain-odbc)

(defvar *open-connections* nil)


;; the main class, this a a wrapper around the connection
;; handle from odbc
;; an instance can only be created by connecting,
;; via connect, driver-connect or the special connect functions 
;; for oracle, sql-server or access
;; (make-instance 'odbc-connection ...) is useless
;; once a connection is closed it can not be reopened

(defclass odbc-connection ()
  (;; any reason to have more than one henv?
   (henv :initform nil :allocation :class :initarg :henv :accessor henv)
   (hdbc :initform nil :initarg :hdbc :accessor hdbc)
   (connected-p :initform nil :reader CONNECTED-P)
   (server-name :reader server-name)
   (dbms-name :reader dbms-name)
   (user-name :reader :user-name)
   ;; info returned from SQLGetInfo
   (info :initform (make-hash-table) :reader db-info))
  #+cormanlisp (:metaclass cl::class-slot-class))

(defun get-odbc-info (con info-type)
  (with-slots (hdbc info) con
    (or (gethash info-type info)
        (setf (gethash info-type info)
              (%sql-get-info hdbc info-type)))))

;; once a connection is created, fill some further fields

(defun build-connection-string (list)
  (if (null list)
    ""
    (let ((sos (make-string-output-stream)))
      (dolist (e list)
        (write-string (string (car e)) sos)
        (write-string "=" sos)
        (write-string (string (cdr e)) sos)
        (write-string ";" sos))
      (let ((res (get-output-stream-string sos)))
        (subseq res 0 (- (length res) 1)))))) 

(defun driver-connect (connection-string)
 (let ((con (make-instance 'odbc-connection)))
   (unless (henv con) ;; has class allocation!
     (setf (henv con) (%new-environment-handle)))
   (setf (hdbc con) (%new-db-connection-handle (henv con)))
   (let ((completion (%sql-driver-connect 
                      (henv con) (hdbc con)  connection-string
                      :noprompt)))
     (declare (ignore completion))
     ;;;
     (push con *open-connections*)
     (setf (slot-value con 'connected-p) t)
     (let* ((server-name (get-odbc-info con $SQL_SERVER_NAME))
            (dbms-name (get-odbc-info con $SQL_DBMS_NAME))
            (is-txn-capable (/= (get-odbc-info con $SQL_TXN_CAPABLE) 
                               $SQL_TC_NONE))
            (user-name
               ;; rav
               ;; for mysql/unoixodbc (get-odbc-info con $SQL_USER_NAME) core dumps
               ;; not known if this is mysql specific or unixodbc specific
               (if (equalp dbms-name "MySQL")
                 "unknown"
                 (get-odbc-info con $SQL_USER_NAME))))
             
       (when is-txn-capable
         (disable-autocommit (hdbc con)))
       (setf (slot-value con 'server-name) server-name)
       (setf (slot-value con 'dbms-name) dbms-name)
       (setf (slot-value con 'user-name) user-name)
       con))))

(defun connect (dsn userid password)
  (let ((str (build-connection-string (list (cons :dsn dsn)
                                           (cons :uid userid)
                                           (cons :pwd password)))))
    (driver-connect str)))

(defun connect-generic (&rest attrs)
  (unless (evenp (length attrs))
    (error "keyword value pairs required"))
    (let ((connection-string  
            (build-connection-string
             (do ((res nil) (attrs attrs (cddr attrs)))
                 ((null attrs) (reverse res))
               (unless (keywordp (car attrs))
                 (error "expected a keword"))
               (unless (stringp (cadr attrs))
                 (error "expected a string"))
               (push (cons (car attrs) (cadr attrs)) res)))))
      (driver-connect connection-string)))


;;; fixme, is this correct?
(defmethod print-object ((connection odbc-connection) s)
  (format s "#<~A SERVER=~S DBMS=~S USER=~S>"
          (class-name (class-of connection))
          (if (slot-boundp connection 'server-name) 
            (slot-value connection 'server-name)
            "")
          (if (slot-boundp connection 'dbms-name) 
            (slot-value connection 'dbms-name)
            "")
          (if (slot-boundp connection 'user-name) 
            (slot-value connection 'user-name)
            "")))



;; before the connection is closed, it is rolled back.
;; odbc complains if a connection with an uncomitted transaction
;; is closed. Commiting the transaction would be wrong.

(defun close-connection (con)
  (when (connected-p con)
    ;; if there is an active transaction and we disconnect then
    ;; odbc creates an error message
    ;; anyway, a transaction must be explicitly commited
    (rollback con)
    (%disconnect (hdbc con))
    (SLOT-MAKUNBOUND con 'hdbc)
    (setf (slot-value con 'connected-p) nil)
    (setf *open-connections* (remove con *open-connections*))
    nil))

(defun trace-connection (con filename)
  (%start-connection-trace (hdbc con) filename)
  nil)

(defun untrace-connection (coN)
  (%stop-connection-trace (hdbc con))
  nil) 


(defun commit (con)
  (%commit (henv con) (hdbc con))
  nil)

(defun rollback (con)
  (%rollback (henv con) (hdbc con))
  nil)

;; this class is a wrapper for odbc statement handles.
;; it is visible to the user only as prepared statement
;; the only way to create a odbc-query is to create a 
;; prepared statement

(defclass odbc-query ()
  ((connection :initarg :connection :reader connection)
   (active-p :initarg :active-p :initform nil)
   (hstmt :initform nil :initarg :hstmt :accessor hstmt) ; = cursor??
   (columns :initform nil)
   (column-count :initform nil :accessor column-count)))

(defun make-query (con)
  (let ((new-query (make-instance 'odbc-query
                                  :connection con
                                  :active-p nil
                                  ;; column-count should be nil
                                  ;;(clone-database database)
                                  :active-p t)))
   (setf (hstmt new-query) (%new-statement-handle (hdbc con)))
   new-query))


(defclass prepared-statement (odbc-query)
  ((parameters :initarg :parameters :accessor parameters)))

(defun make-prepared-statement (con)
  (let ((new-query (make-instance 'prepared-statement
                                  :connection con
                                  :active-p nil
                                  ;; column-count should be nil
                                  ;;(clone-database database)
                                  :active-p t)))
   (setf (hstmt new-query) (%new-statement-handle (hdbc con)))
   new-query))




(defun column-names (query)
  (let ((columns (slot-value query 'columns))
        (res nil))
    (dotimes (i (length columns))
      (push (slot-value (aref columns i) 'column-name) res))
    (nreverse res)))

(defun bind-columns (query)
  (with-slots (hstmt 
               columns
               column-count) 
              query 
    (let ((cc (result-columns-count hstmt)))
      (when (zerop cc)
        (error "can not bind columns, there is no result set"))
      (setf column-count cc)
      (setf columns (make-array column-count))
      (dotimes (pos column-count)
        ;; the columns are 0 based, at least here
        (let ((col (create-column hstmt pos)))
          (setf (aref columns pos) col))))))

(defun unbind-columns (query)
  (let ((columns (slot-value query 'columns))
        (hstmt (slot-value query 'hstmt)))
    (when columns
      (dotimes (i (length columns))
        (let ((column (aref columns i)))
          (when column
            (with-slots (value-ptr ind-ptr) column
              (when value-ptr
                (%dispose-ptr value-ptr))
              (when ind-ptr
                (%dispose-ptr ind-ptr))))))
      (setf (slot-value query 'columns) nil)
      (when hstmt
        (%free-statement hstmt :unbind)))))
  
(defmethod free-query ((query odbc-query))
  (unbind-columns query)
  (%free-statement (hstmt query) :close)
  (%free-statement (hstmt query) :drop)
  (SLOT-MAKUNBOUND query 'hstmt))

(defmethod free-query ((query prepared-statement))
  (free-parameters query)
  (call-next-method))

(defun free-statement (x)
  (free-query x))


(defmethod fetch-query-results ((query odbc-query))
  (with-slots (hstmt columns column-count)
      query
      (let ((res nil))
        (loop
          (when (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
            (return-from fetch-query-results (nreverse res)))
          (let ((row nil))
            (dotimes (i column-count)
              (push (get-column-value (aref columns i)) row))
            (push (nreverse row) res))))))

;;; on hold
#+ignore
(defmethod fetch-row ((query odbc-query))
  (with-slots (hstmt columns column-count)
      query
    (if (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
      nil
      (let ((row nil))
        (dotimes (i column-count)
          (push (get-column-value (aref columns i)) row))
        (nreverse row)))))

;;; direct sql
(defun exec-query (con str)
  (let ((query (make-query con)))
    (unwind-protect 
      (progn
        (%sql-exec-direct str (hstmt query) (henv con) (hdbc con))
        (bind-columns query)
        ;(break)
        (let ((res (fetch-query-results query ))
              (names (coerce (column-names query) 'list)))
          (values res names)))
      (free-query query)
      )))

#+ignore
(defun exec-query-new (connection sql parameter-list)
  (with-slots (hdbc) connection
    (let* ((query (make-query connection))
           (pos 0)
           (hstmt (hstmt query)))
      ;; only make-query could have failed, and then there would be no 
      ;; query to free
      (unwind-protect
        (progn
          (setf (parameters query) (make-array (length parameter-list) 
                                               :initial-element nil))
          (dolist (param parameter-list)
            (multiple-value-bind (type direction args)
                (if (and param (symbolp param))
                  (values param :in nil)
                  (values (first param)
                          (second param)
                          (rest (rest param))))
              (let ((parameter (create-parameter query pos type direction args)))
            (bind-parameter hstmt pos parameter)
            (setf (aref (parameters query) pos) parameter) 
            (incf pos))))
          (%sql-exec-direct sql (hstmt query) (henv connection) (hdbc connection))
          (bind-columns query)
          (let ((res (fetch-query-results query ))
                (names (coerce (column-names query) 'list)))
            (values res names)))
          (free-query query)))))

#+ignore
(defun exec-query-n (con str)
  (let ((query (make-query con))
        (res-list nil))
    (unwind-protect 
      (progn
        (%sql-exec-direct str (hstmt query) (henv con) (hdbc con))
        (loop
          (bind-columns query)                              ;(break)
          (let ((res (fetch-query-results query ))
                (names (coerce (column-names query) 'list)))
            (push (list res names) res-list)
            (unbind-columns query)
            (unless (%sql-more-results (hstmt query))
               (return)))))
      (free-query query))
    (reverse res-list)
    ))
  

; (exec-query-2 *con2* "select 1;select 2")



(defmethod exec-update (con sql-string)
  (let ((query (make-query con)))
    (unwind-protect 
      (with-slots (hstmt) query
        (with-slots (henv hdbc) con
          (%sql-exec-direct sql-string hstmt henv hdbc)
          (let ((res (result-rows-count hstmt)))
            (if (= res -1) nil res))))
      (free-query query))))


(defmethod exec-command (con sql-string)
  (let ((query (make-query con)))
    (unwind-protect
      (with-slots (hstmt) query
        (with-slots (henv hdbc) con
          (%sql-exec-direct sql-string hstmt henv hdbc)
          (let ((res (result-rows-count hstmt)))
            (if (= res -1) nil res))))
      (free-query query))))

;;; parameterized sql

(defmethod prepare-statement ((connection odbc-connection) sql parameter-list)
  (with-slots (hdbc) connection
    (let* ((query (make-prepared-statement connection))
           (pos 0)
           (hstmt (hstmt query)))
      (%sql-prepare hstmt sql)
      (setf (parameters query) (make-array (length parameter-list) 
                                           :initial-element nil))
      (dolist (param parameter-list)
        (multiple-value-bind (type direction args)
            (if (and param (symbolp param))
                (values param :in nil)
              (values (first param)
                      (second param)
                      (rest (rest param))))
          (let ((parameter (create-parameter query pos type direction args)))
            (bind-parameter hstmt pos parameter)
            (setf (aref (parameters query) pos) parameter) 
            (incf pos))))
      query)))

(defmethod set-parameters ((query odbc-query) parameter-values)
  (let ((i 0)
        (parameters (parameters query)))
    (dolist (pval parameter-values)
      (loop
        (when (>= i (length parameters))
          (error "too many actual parameters"))
        (if (eql (slot-value (aref parameters i) 'direction) :out)
          (incf i)
          (return)))
      (let* ((parameter (aref parameters i))) 
        (set-parameter-value parameter pval)
        (incf i)))
    (do ((j i (+ j 1)))
        ((>= j (length parameters)))
      (unless (eql :out (slot-value (aref parameters j) 'direction))
        (error "not enough actual parameters")))))

(defmethod get-parameters ((query odbc-query))
  (let ((res nil)
        (parameters (parameters query)))
    (dotimes (i (length parameters))
      (let* ((parameter (aref parameters i))
             (direction (slot-value parameter 'direction)))
        (when (or (eq direction :out) (eq direction :inout))
          (push (get-parameter-value parameter) res))))
    (nreverse res)))


(defmethod set-params-and-exec ((query odbc-query) parameters)
  (set-parameters query parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect
      (let ((res (%sql-execute (hstmt query)))
            (last-pos nil))
        (if (= res $SQL_NEED_DATA)
          (loop
            (multiple-value-bind (res pos)
                (sql-param-data-position hstmt)
              (unless (= res $SQL_NEED_DATA)
                (return)) ; from the loop
              (when (eql pos last-pos)
                (error "paramter ~A is not filled yet" pos))
              (setf last-pos pos)
              (let ((param (aref (slot-value query 'parameters) pos)))
                (send-parameter-data param hstmt)))))))))

;; this functions works only, since we store at 
;; value-ptr the position of the parameter
(defun sql-param-data-position (hstmt)  
  (%with-temporary-allocation 
      ((ptr :ptr))
  (let ((res (with-error-handling (:hstmt hstmt) (%sql-param-data hstmt ptr))))
    (values res (if (= res $SQL_NEED_DATA) 
                  (%get-long (%get-ptr ptr)))))))

(defmethod exec-prepared-query ((query prepared-statement) parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
    ;;; fixme
        ;; if this query has already been used, we reuse the old
        ;; column binding. This breaks down if the schema has been changed
        ;; in the mean time, ex. the query is "select * from table1" and then
        ;; a column of table1 is dropped.
        (let ((no-of-columns (result-columns-count hstmt)))
          (when (zerop no-of-columns )
            (error "there is no result set"))
          (if (column-count query)
            (unless (= (column-count query) no-of-columns)
              (error "the number of columns has changed"))
            (bind-columns query)))
        (values (fetch-query-results query)
                (coerce (column-names query) 'list)))
    (%free-statement hstmt :close))))


(defmethod exec-prepared-update ((query prepared-statement) parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
        (let ((rowcount (result-rows-count hstmt)))
          (if (= rowcount -1) nil rowcount)))
      (%free-statement hstmt :close))))

(defmethod exec-prepared-command ((query prepared-statement) parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
        (get-parameters query))
      (%free-statement hstmt :close))))
