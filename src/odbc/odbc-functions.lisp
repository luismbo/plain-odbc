;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LispWorks, CMUCL, ACL and CormanLisp
;; Version 0.9
;; Copyright (C) Paul Meurer 1999, 2000. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.



(in-package :plain-odbc)

(defmacro with-temporary-allocations (allocs &body body)
  `(let (,@allocs)
    (unwind-protect
      (progn ,@body)
      ,@(mapcar (lambda (alloc) (list 'uffi:free-foreign-object (first alloc))) allocs))))

             


;;; rav:
;;; primitive error handling
;;; maybe one could create sub conditions based on the sql-state?
;;; but not even jdbc does this

(define-condition sql-condition (condition)
  ((error-message :initarg :error-message)
   (error-code :initarg :error-code)
   (sql-state :initarg :sql-state))
  (:report (lambda (condition stream) 
             (format stream "~A, error code ~A, State: ~A." 
                     (slot-value condition 'error-message)
                     (slot-value condition 'error-code)
                     (slot-value condition 'sql-state)))))

(define-condition sql-error (sql-condition error)
  ())

(define-condition sql-warning (sql-condition warning)
  ())

(defun handle-error (henv hdbc hstmt)
   (let
       ((sql-state (uffi:allocate-foreign-string 256))
        (error-message (uffi:allocate-foreign-string #.$SQL_MAX_MESSAGE_LENGTH))
        (error-code (allocate-foreign-object :long))
        (msg-length (allocate-foreign-object :short)))
     ;; fixme, remove
     (SQLError henv 
               hdbc 
               hstmt sql-state
               error-code error-message
               $SQL_MAX_MESSAGE_LENGTH msg-length)
     ;;;fixme, remove
     (values
      (uffi:convert-from-foreign-string error-message)
      (uffi:convert-from-foreign-string sql-state)
      (uffi:deref-pointer msg-length :short)
      (uffi:deref-pointer error-code :long))))


; test this: return a keyword for efficiency
;; rav,
;; problem: calling SQLError clears the error state
;#+ignore
(defun sql-state (henv hdbc hstmt)
  (with-temporary-allocations 
      ((sql-state (uffi:allocate-foreign-string 256))
       (error-message (uffi:allocate-foreign-string $SQL_MAX_MESSAGE_LENGTH))
       (error-code (uffi:allocate-foreign-object :long))
       (msg-length (uffi:allocate-foreign-object :short)))
    (SQLError henv hdbc hstmt sql-state error-code
              error-message $SQL_MAX_MESSAGE_LENGTH msg-length)
    (%get-string sql-state 5) ;(%cstring-to-keyword sql-state)
    ))

;;; rav:
;;; instead of a big macro use a fucntion

(defun error-handling-fun (result-code henv hdbc hstmt)
  ;; *** is this a bug in allegro or in my code??
  ;#+allegro (setf result-code (short-to-signed-short result-code))

  (case result-code 
    (#.$SQL_SUCCESS (values result-code nil))
    ((#.$SQL_SUCCESS_WITH_INFO #.$SQL_ERROR)
      (multiple-value-bind (error-message sql-state msg-length error-code)
          (handle-error (or henv (%null-ptr))
                        (or hdbc (%null-ptr))
                        (or hstmt (%null-ptr)))
        (declare (ignore msg-length)) 
        (values result-code
                (make-condition
                 (if (eql result-code #.$SQL_SUCCESS_WITH_INFO)
                   'sql-warning
                   'sql-error)
                 :error-message error-message
                 :sql-state sql-state
                 :error-code error-code))))
    
    (#.$SQL_INVALID_HANDLE
      (values result-code
              (make-condition 'sql-error :error-message "[ODBC error] Invalid handle")))
    (#.$SQL_STILL_EXECUTING
      (values result-code 
              (make-condition 'sql-error :error-message"[ODBC error] Still executing")))
    (otherwise (values result-code nil))
    ))


;;; rav:
;;; but the remaining macro is still large
#+ignore
(defmacro with-error-handling ((&key henv hdbc hstmt (print-info t))
                                   odbc-call &body body)
  (declare (ignore print-info))
  (let ((condition-var (gensym))
        (result-code (gensym)))
    `(multiple-value-bind (,result-code ,condition-var) 
      (error-handling-fun ,odbc-call ,henv ,hdbc ,hstmt)
      (typecase ,condition-var 
        (warning (warn ,condition-var))
        (error (error ,condition-var)))
      ,result-code ,@body)))


;;; rav:
;; the original macro
#-ignore 
(defmacro with-error-handling ((&key henv hdbc hstmt (print-info t))
                                   odbc-call &body body)
  (let ((result-code (gensym)))
    `(let ((,result-code ,odbc-call))
       ;; *** is this a bug in allegro or in my code??
       ;#+allegro (setf ,result-code (short-to-signed-short ,result-code))
       (case ,result-code
         (#.$SQL_SUCCESS
          (progn ,result-code ,@body))
         (#.$SQL_SUCCESS_WITH_INFO
          (when ,print-info
            (multiple-value-bind (error-message sql-state)
                                 (handle-error (or ,henv (%null-ptr))
                                               (or ,hdbc (%null-ptr))
                                               (or ,hstmt (%null-ptr)))
              (warn "[ODBC info] ~a state: ~a"
		    ,result-code error-message
		    sql-state)))
          (progn ,result-code ,@body))
         (#.$SQL_INVALID_HANDLE
          (error "[ODBC error] Invalid handle"))
         (#.$SQL_STILL_EXECUTING
          (error "[ODBC error] Still executing"))
         (#.$SQL_ERROR
          (multiple-value-bind (error-message sql-state)
                               (handle-error (or ,henv (%null-ptr))
                                             (or ,hdbc (%null-ptr))
                                             (or ,hstmt (%null-ptr)))
            (error "[ODBC error] ~a; state: ~a" error-message sql-state)))
         (otherwise
           (progn ,result-code ,@body))
         ))))


(defun %new-environment-handle ()
  (uffi:with-foreign-object (phenv 'sql-handle)
    (with-error-handling
      ()
      (SQLAllocEnv phenv)
      (uffi:deref-pointer phenv 'sql-handle)
      )))

(defun %sql-free-environment (henv)
  (with-error-handling 
    (:henv henv)
    (SQLFreeEnv henv)))

(defun %new-db-connection-handle (henv)
  (uffi:with-foreign-object (phdbc 'sql-handle)  
    (with-error-handling
        (:henv henv)
      (SQLAllocConnect henv phdbc)
      (deref-pointer phdbc 'sql-handle))))

(defun %free-statement (hstmt option)
  (with-error-handling 
      (:hstmt hstmt)
      (SQLFreeStmt 
       hstmt 
       (ecase option
         (:drop $SQL_DROP)
         (:close $SQL_CLOSE)
         (:unbind $SQL_UNBIND)
         (:reset $SQL_RESET_PARAMS)))))

(defmacro with-statement-handle ((hstmt hdbc) &body body)
  `(let ((,hstmt (%new-statement-handle ,hdbc)))
     (unwind-protect
       (progn ,@body)
       (%free-statement ,hstmt :drop))))

;;; rav: ignored
#+ignore
(defmacro %with-transaction ((henv hdbc) &body body)
  (let ((successp (gensym)))
    `(let ((,successp nil))
       (unwind-protect
         (prog1
           (progn ,@body)
           (setf ,successp t))
         (with-error-handling (:henv ,henv :hdbc ,hdbc)
           (SQLTransact 
            ,henv ,hdbc 
            (if ,successp $SQL_COMMIT $SQL_ROLLBACK)))))))

;; functional interface

;;; fixme , with c-string?
(defun %sql-connect (hdbc server uid pwd)
  (uffi:with-cstring (server-ptr server)
    (uffi:with-cstring (uid-ptr uid)
      (uffi:with-cstring (pwd-ptr pwd)
        (with-error-handling 
            (:hdbc hdbc)
      (SQLConnect hdbc server-ptr $SQL_NTS uid-ptr 
                  $SQL_NTS pwd-ptr $SQL_NTS))))))

;;
(defun %sql-driver-connect (henv hdbc connection-string completion-option)
  (let ((completion-option
         (ecase completion-option
           (:complete $SQL_DRIVER_COMPLETE)
           (:required $SQL_DRIVER_COMPLETE_REQUIRED)
           (:prompt $SQL_DRIVER_PROMPT)
           (:noprompt $SQL_DRIVER_NOPROMPT))))
    (with-temporary-allocations
        ((connection-str-ptr (uffi:convert-to-foreign-string connection-string))
         (complete-connection-str-ptr (uffi:allocate-foreign-string 1024))
         (length-ptr (uffi:allocate-foreign-object :short)))
      (with-error-handling 
          (:henv henv :hdbc hdbc)
        
        (SQLDriverConnect hdbc 
                          (%null-ptr) ; no window
                            connection-str-ptr 
                            (length connection-string)
                                        ;$SQL_NTS
                            complete-connection-str-ptr 1024
                            length-ptr completion-option))
      (uffi:convert-from-foreign-string complete-connection-str-ptr))))

(defun %disconnect (hdbc)
  (with-error-handling 
    (:hdbc hdbc)
    (SQLDisconnect hdbc)))

(defun %commit (henv hdbc)
  (with-error-handling 
    (:henv henv :hdbc hdbc)
    (SQLTransact 
     henv hdbc $SQL_COMMIT)))

(defun %rollback (henv hdbc)
  (with-error-handling 
    (:henv henv :hdbc hdbc)
    (SQLTransact 
     henv hdbc $SQL_ROLLBACK)))

; col-nr is zero-based in Lisp
; col-nr = :bookmark retrieves a bookmark.
(defun %bind-column (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (with-error-handling
    (:hstmt hstmt)
    (SQLBindCol hstmt
                (if (eq column-nr :bookmark) 0 (1+ column-nr))
                c-type data-ptr precision out-len-ptr)))

; parameter-nr is zero-based in Lisp
(defun %sql-bind-parameter (hstmt parameter-nr parameter-type c-type
                                      sql-type precision scale data-ptr
                                      max-value out-len-ptr)
  (with-error-handling
    (:hstmt hstmt)
    (SQLBindParameter hstmt (1+ parameter-nr)
                      parameter-type ;$SQL_PARAM_INPUT 
                      c-type ;$SQL_C_CHAR
                      sql-type ;$SQL_VARCHAR
                      precision ;(1- (length str))
                      scale ;0
                      data-ptr
                      max-value
                      out-len-ptr ;#.(%null-ptr)
                      )))

(defun %sql-fetch (hstmt)
  (with-error-handling 
      (:hstmt hstmt)
      (SQLFetch hstmt)))

(defun %new-statement-handle (hdbc)
  (with-temporary-allocations  
      ((hstmt-ptr (uffi:allocate-foreign-object 'sql-handle)))
    (with-error-handling 
        (:hdbc hdbc)
      (SQLAllocStmt hdbc hstmt-ptr) 
      (uffi:deref-pointer hstmt-ptr 'sql-handle))))

(defun %sql-get-info (hdbc info-type)
  (ecase info-type
    ;; those return string
    ((#.$SQL_ACCESSIBLE_PROCEDURES
      #.$SQL_ACCESSIBLE_TABLES
      #.$SQL_COLUMN_ALIAS
      #.$SQL_DATA_SOURCE_NAME
      #.$SQL_DATA_SOURCE_READ_ONLY
      #.$SQL_DBMS_NAME
      #.$SQL_DBMS_VER
      #.$SQL_DRIVER_NAME
      #.$SQL_DRIVER_ODBC_VER
      #.$SQL_DRIVER_VER
      #.$SQL_EXPRESSIONS_IN_ORDERBY
      #.$SQL_IDENTIFIER_QUOTE_CHAR
      #.$SQL_KEYWORDS
      #.$SQL_LIKE_ESCAPE_CLAUSE
      #.$SQL_MAX_ROW_SIZE_INCLUDES_LONG
      #.$SQL_MULT_RESULT_SETS
      #.$SQL_MULTIPLE_ACTIVE_TXN
      #.$SQL_NEED_LONG_DATA_LEN
      #.$SQL_ODBC_SQL_OPT_IEF
      #.$SQL_ODBC_VER
      #.$SQL_ORDER_BY_COLUMNS_IN_SELECT
      #.$SQL_OUTER_JOINS
      #.$SQL_OWNER_TERM
      #.$SQL_PROCEDURE_TERM
      #.$SQL_PROCEDURES
      #.$SQL_QUALIFIER_NAME_SEPARATOR
      #.$SQL_QUALIFIER_TERM
      #.$SQL_ROW_UPDATES
      #.$SQL_SEARCH_PATTERN_ESCAPE
      #.$SQL_SERVER_NAME
      #.$SQL_SPECIAL_CHARACTERS
      #.$SQL_TABLE_TERM
      #.$SQL_USER_NAME)
     (with-temporary-allocations 
         ((info-ptr (uffi:allocate-foreign-string 1024))
          (info-length-ptr (uffi:allocate-foreign-object :short)))
       (with-error-handling 
         (:hdbc hdbc)
	 #-pcl
         (SQLGetInfo hdbc info-type info-ptr 1023 info-length-ptr)
	 #+pcl
         (SQLGetInfo-Str hdbc info-type info-ptr 1023 info-length-ptr)
         (uffi:convert-from-foreign-string info-ptr))))
    ;; those returning a word
    ((#.$SQL_ACTIVE_CONNECTIONS
      #.$SQL_ACTIVE_STATEMENTS
      #.$SQL_CONCAT_NULL_BEHAVIOR
      #.$SQL_CORRELATION_NAME
      #.$SQL_CURSOR_COMMIT_BEHAVIOR
      #.$SQL_CURSOR_ROLLBACK_BEHAVIOR
      #.$SQL_MAX_COLUMN_NAME_LEN
      #.$SQL_MAX_COLUMNS_IN_GROUP_BY
      #.$SQL_MAX_COLUMNS_IN_INDEX
      #.$SQL_MAX_COLUMNS_IN_ORDER_BY
      #.$SQL_MAX_COLUMNS_IN_SELECT
      #.$SQL_MAX_COLUMNS_IN_TABLE
      #.$SQL_MAX_CURSOR_NAME_LEN
      #.$SQL_MAX_OWNER_NAME_LEN
      #.$SQL_MAX_PROCEDURE_NAME_LEN
      #.$SQL_MAX_QUALIFIER_NAME_LEN
      #.$SQL_MAX_TABLE_NAME_LEN
      #.$SQL_MAX_TABLES_IN_SELECT
      #.$SQL_MAX_USER_NAME_LEN
      #.$SQL_NON_NULLABLE_COLUMNS
      #.$SQL_NULL_COLLATION
      #.$SQL_ODBC_API_CONFORMANCE
      #.$SQL_ODBC_SAG_CLI_CONFORMANCE
      #.$SQL_ODBC_SQL_CONFORMANCE
      #.$SQL_QUALIFIER_LOCATION
      #.$SQL_QUOTED_IDENTIFIER_CASE
      #.$SQL_TXN_CAPABLE)
     (with-temporary-allocations
         ((info-ptr (uffi::allocate-foreign-object :short))
          (info-length-ptr (uffi::allocate-foreign-object :short)))
       (with-error-handling 
	(:hdbc hdbc)
         (SQLGetInfo hdbc
		     info-type
		     info-ptr
		     255
		     info-length-ptr)
         (uffi:deref-pointer info-ptr :short)))
     )
    ;; those returning a long bitmask
    ((#.$SQL_ALTER_TABLE 
      #.$SQL_BOOKMARK_PERSISTENCE
      #.$SQL_CONVERT_BIGINT
      #.$SQL_CONVERT_BINARY
      #.$SQL_CONVERT_BIT
      #.$SQL_CONVERT_CHAR
      #.$SQL_CONVERT_DATE
      #.$SQL_CONVERT_DECIMAL
      #.$SQL_CONVERT_DOUBLE
      #.$SQL_CONVERT_FLOAT
      #.$SQL_CONVERT_INTEGER
      #.$SQL_CONVERT_LONGVARCHAR
      #.$SQL_CONVERT_NUMERIC
      #.$SQL_CONVERT_REAL
      #.$SQL_CONVERT_SMALLINT
      #.$SQL_CONVERT_TIME
      #.$SQL_CONVERT_TIMESTAMP
      #.$SQL_CONVERT_TINYINT
      #.$SQL_CONVERT_VARBINARY
      #.$SQL_CONVERT_VARCHAR
      #.$SQL_CONVERT_LONGVARBINARY
      #.$SQL_CONVERT_FUNCTIONS
      #.$SQL_FETCH_DIRECTION
      #.$SQL_FILE_USAGE
      #.$SQL_GETDATA_EXTENSIONS
      #.$SQL_LOCK_TYPES
      #.$SQL_MAX_INDEX_SIZE
      #.$SQL_MAX_ROW_SIZE
      #.$SQL_MAX_STATEMENT_LEN
      #.$SQL_NUMERIC_FUNCTIONS
      #.$SQL_OWNER_USAGE
      #.$SQL_POS_OPERATIONS
      #.$SQL_POSITIONED_STATEMENTS
      #.$SQL_QUALIFIER_USAGE
      #.$SQL_SCROLL_CONCURRENCY
      #.$SQL_SCROLL_OPTIONS
      #.$SQL_STATIC_SENSITIVITY
      #.$SQL_STRING_FUNCTIONS
      #.$SQL_SUBQUERIES
      #.$SQL_SYSTEM_FUNCTIONS
      #.$SQL_TIMEDATE_ADD_INTERVALS
      #.$SQL_TIMEDATE_DIFF_INTERVALS
      #.$SQL_TIMEDATE_FUNCTIONS
      #.$SQL_TXN_ISOLATION_OPTION
      #.$SQL_UNION)
      (with-temporary-allocations 
          ((info-ptr (uffi:allocate-foreign-object :unsigned-long))
           (info-length-ptr (uffi:allocate-foreign-object :short)))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc
		     info-type
		     info-ptr
		     255
		     info-length-ptr)
         (uffi:deref-pointer info-ptr :unsigned-long)))
     )
    ;; those returning a long integer
    ((#.$SQL_DEFAULT_TXN_ISOLATION
      #.$SQL_DRIVER_HDBC
      #.$SQL_DRIVER_HENV
      #.$SQL_DRIVER_HLIB
      #.$SQL_DRIVER_HSTMT
      #.$SQL_GROUP_BY
      #.$SQL_IDENTIFIER_CASE
      #.$SQL_MAX_BINARY_LITERAL_LEN
      #.$SQL_MAX_CHAR_LITERAL_LEN
      #.$SQL_ACTIVE_ENVIRONMENTS
      )
     (with-temporary-allocations 
         ((info-ptr (uffi:allocate-foreign-object :long))
          (info-length-ptr (uffi:allocate-foreign-object :short)))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc info-type info-ptr 255 info-length-ptr)
         (uffi:deref-pointer info-ptr :unsigned-long))))))

(defun %sql-exec-direct (sql hstmt henv hdbc)
  (uffi:with-cstring (sql-ptr sql)
    (with-error-handling
        (:hstmt hstmt :henv henv :hdbc hdbc)
      (SQLExecDirect hstmt sql-ptr $SQL_NTS))))

(defun %sql-cancel (hstmt)
  (with-error-handling
    (:hstmt hstmt)
    (SQLCancel hstmt)))

(defun %sql-execute (hstmt)
  (with-error-handling
    (:hstmt hstmt)
    (SQLExecute hstmt)))

(defun result-columns-count (hstmt)
  (with-temporary-allocations 
      ((columns-nr-ptr (uffi:allocate-foreign-object :short)))
    (with-error-handling (:hstmt hstmt)
                         (SQLNumResultCols hstmt columns-nr-ptr)
      (uffi:deref-pointer columns-nr-ptr :short))))

(defun result-rows-count (hstmt)
  (with-temporary-allocations 
      ((row-count-ptr (uffi:allocate-foreign-object :long)))
    (with-error-handling (:hstmt hstmt)
                         (SQLRowCount hstmt row-count-ptr)
      (uffi:deref-pointer row-count-ptr :long))))


;;; fixme, the whole column descriptiopn stuff should be put into one loop
;;; so that we can reuse the allocations

;; Column counting is 1-based
(defun %describe-column (hstmt column-nr)
  (with-temporary-allocations ((column-name-ptr (uffi:allocate-foreign-string 256))
                               (column-name-length-ptr (uffi:allocate-foreign-object :short))
                               (column-sql-type-ptr (uffi:allocate-foreign-object :short))
                               (column-precision-ptr (uffi:allocate-foreign-object :unsigned-long))
                               (column-scale-ptr (uffi:allocate-foreign-object :short))
                               (column-nullable-p-ptr (uffi:allocate-foreign-object :short)))
    (with-error-handling (:hstmt hstmt)
                         (SQLDescribeCol hstmt column-nr column-name-ptr 256
                                         column-name-length-ptr
                                         column-sql-type-ptr
                                         column-precision-ptr
                                         column-scale-ptr
                                         column-nullable-p-ptr)
      (values
       (uffi:convert-from-foreign-string column-name-ptr)
       (uffi:deref-pointer column-sql-type-ptr :short)
       (uffi:deref-pointer column-precision-ptr :unsigned-long)
       (uffi:deref-pointer column-scale-ptr :short)
       (uffi:deref-pointer column-nullable-p-ptr :short)))))

;; parameter counting is 1-based
(defun %describe-parameter (hstmt parameter-nr)
  (with-temporary-allocations ((column-sql-type-ptr (uffi:allocate-foreign-object :short))
                               (column-precision-ptr (uffi:allocate-foreign-object :long))
                               (column-scale-ptr (uffi:allocate-foreign-object :short))
                               (column-nullable-p-ptr (uffi:allocate-foreign-object :short)))
    (with-error-handling 
      (:hstmt hstmt)
      (SQLDescribeParam hstmt parameter-nr
			column-sql-type-ptr
                        column-precision-ptr
			column-scale-ptr
                        column-nullable-p-ptr)
      (values
       (uffi:deref-pointer column-sql-type-ptr :short)
       (uffi:deref-pointer column-precision-ptr :unsigned-long)
       (uffi:deref-pointer column-scale-ptr :short)
       (uffi:deref-pointer column-nullable-p-ptr :short)))))

(defun %column-attributes (hstmt column-nr descriptor-type)
  (with-temporary-allocations 
      ((descriptor-info-ptr (uffi:allocate-foreign-string  256))
       (descriptor-length-ptr (uffi:allocate-foreign-object :short))
       (numeric-descriptor-ptr (uffi:allocate-foreign-object :long)))
    (with-error-handling
      (:hstmt hstmt) 
      (SQLColAttributes hstmt column-nr descriptor-type descriptor-info-ptr 256
                        descriptor-length-ptr
			numeric-descriptor-ptr)
      (values
       (uffi:convert-from-foreign-string descriptor-info-ptr)
       (uffi:deref-pointer numeric-descriptor-ptr :long)))))


;; fixme, include it later
#+ignore
(defun %prepare-describe-columns (hstmt table-qualifier table-owner 
                                   table-name column-name)
  (with-tempo (table-qualifier-ptr table-qualifier)
    (with-cstr (table-owner-ptr table-owner) 
      (with-cstr (table-name-ptr table-name)
        (with-cstr (column-name-ptr column-name)
          (with-error-handling
            (:hstmt hstmt) 
            (SQLColumns hstmt
                        table-qualifier-ptr (length table-qualifier)
                        table-owner-ptr (length table-owner)
                        table-name-ptr (length table-name)
                        column-name-ptr (length column-name))))))))

;;; rav, currently not needed
;; resultset returning odbc functions should be
;; integrated with the query objects

#+ignore
(defun %describe-columns (hdbc table-qualifier table-owner 
                                   table-name column-name)
  (with-statement-handle (hstmt hdbc)
    (%prepare-describe-columns hstmt table-qualifier table-owner 
                               table-name column-name)
    (fetch-all-rows hstmt)))

(defun %sql-data-sources (henv &key (direction :first))
  (with-temporary-allocations 
      ((name-ptr (uffi:allocate-foreign-string (1+ $SQL_MAX_DSN_LENGTH)))
       (name-length-ptr (uffi:allocate-foreign-object :short))
       (description-ptr (uffi:allocate-foreign-string 1024))
       (description-length-ptr (uffi:allocate-foreign-object :short)))
    (let ((res (with-error-handling
                   (:henv henv)
                 (SQLDataSources henv
                                 (ecase direction
                                   (:first $SQL_FETCH_FIRST)
                                   (:next $SQL_FETCH_NEXT))
                                 name-ptr
                                 (1+ $SQL_MAX_DSN_LENGTH)
                                 name-length-ptr
                                 description-ptr
                                 1024
                                 description-length-ptr))))
      (unless (= res $SQL_NO_DATA_FOUND)
        (values (uffi:convert-from-foreign-string name-ptr)
                (uffi:convert-from-foreign-string description-ptr))))))


(defun %sql-prepare (hstmt sql)
  (uffi:with-cstring (sql-ptr sql)
    (with-error-handling (:hstmt hstmt)
      (SQLPrepare hstmt sql-ptr $SQL_NTS))))

;; depending on option, we return a long int or a string; string not implemented
(defun get-connection-option (hdbc option)
  (with-temporary-allocations 
      ((param-ptr (allocate-foreign-object  :long))) ;#+ignore #.(1+ $SQL_MAX_OPTION_STRING_LENGTH)))
    (with-error-handling (:hdbc hdbc)
      (SQLGetConnectOption hdbc option param-ptr)
      (uffi:deref-pointer param-ptr :long))))

(defun set-connection-option (hdbc option param)
  (with-error-handling (:hdbc hdbc)
    (SQLSetConnectOption hdbc option param)))

(defun disable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_OFF))

(defun enable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_ON))


***
;;; rav, 11.6.2005
;;; added tracing support

(defun set-connection-attr-integer (hdbc option val)
  (with-error-handling (:hdbc hdbc)
    (SQLSetConnectAttr_long hdbc option val 0)))

(defun set-connection-attr-string (hdbc option val)
  (with-error-handling (:hdbc hdbc)
    (uffi:with-cstring (ptr val)
      (SQLSetConnectAttr_string hdbc option ptr (length val)))))

(defun %start-connection-trace (hdbc filename)
  (set-connection-attr-string hdbc  $SQL_ATTR_TRACEFILE	filename)
  (set-connection-attr-integer hdbc $SQL_ATTR_TRACE	$SQL_OPT_TRACE_ON))

(defun %stop-connection-trace (hdbc)
  (set-connection-attr-integer hdbc $SQL_ATTR_TRACE	$SQL_OPT_TRACE_OFF))
  

(defun get-connection-attr-integer (hdbc attr)
  (with-temporary-allocations
      ((ptr (uffi:allocate-foreign-object :long))
       (lenptr (uffi:allocate-foreign-object :long)))
    (with-error-handling (:hdbc hdbc)
      (SQLGetConnectAttr hdbc attr ptr 0 lenptr))
    (uffi:deref-pointer ptr :long)))

(defun get-connection-attr-string (hdbc attr)
  (with-temporary-allocations 
      ((ptr (uffi:allocate-foreign-string 256))
       (lenptr (uffi:allocate-foreign-object :long)))
    (with-error-handling (:hdbc hdbc)
      (SQLGetConnectAttr hdbc attr ptr 256 lenptr))
    (uffi:convert-from-foreign-string ptr 
                                      :length (uffi:deref-pointer lenptr :long))))

;;; small test for the get-connection-attr
(defun %get-current-catalog (hdbc)
  (get-connection-attr-string hdbc $SQL_ATTR_CURRENT_CATALOG))

(defun %set-current-catalog (hdbc catalog)
  (set-connection-attr-string hdbc $SQL_ATTR_CURRENT_CATALOG catalog))



(defun %connection-ok-p (hdbc)
  (with-error-handling (:hdbc hdbc)
    (ecase (get-connection-attr-integer hdbc $SQL_ATTR_CONNECTION_DEAD)
      (#.$sql_cd_true nil)
      (#.$sql_cd_false t))))

;;;


(defun %sql-set-pos (hstmt row option lock)
  (with-error-handling 
    (:hstmt hstmt)
    (SQLSetPos hstmt row option lock)))

(defun %sql-extended-fetch (hstmt fetch-type row)
  (with-temporary-allocations 
      ((row-count-ptr (uffi:allocate-foreign-object :unsigned-long))
       (row-status-ptr (uffi:allocate-foreign-object :short)))
    (with-error-handling (:hstmt hstmt)
      (SQLExtendedFetch hstmt fetch-type row row-count-ptr
			row-status-ptr)
      (values (uffi:deref-pointer row-count-ptr :unsigned-long)
              (uffi:deref-pointer row-status-ptr :short)))))

; column-nr is zero-based
(defun %sql-get-data (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (with-error-handling
    (:hstmt hstmt :print-info nil)
    (SQLGetData hstmt (if (eq column-nr :bookmark) 0 (1+ column-nr))
                c-type data-ptr precision out-len-ptr)))

(defun %sql-get-data-raw (hstmt position c-type data-ptr buffer-length ind-ptr)
  (SQLGetData hstmt (1+ position)
              c-type data-ptr buffer-length ind-ptr))


(defun %sql-param-data (hstmt param-ptr)
  (with-error-handling (:hstmt hstmt :print-info t) ;; nil
      (SQLParamData hstmt param-ptr)))


(defun %sql-put-data (hstmt data-ptr size)
  (with-error-handling
      (:hstmt hstmt :print-info t) ;; nil
    (SQLPutData hstmt data-ptr size)))


(defun %sql-more-results (hstmt)
  (let ((res (SQLMoreResults hstmt)))
    (case res
      ((#.$SQL_SUCCESS_WITH_INFO #.$SQL_SUCCESS) t)
      ((#.$SQL_NO_DATA_FOUND) nil)
      (otherwise (error-handling-fun res nil nil hstmt)))))

;(defconstant $sql-data-truncated (intern "01004" :keyword))
