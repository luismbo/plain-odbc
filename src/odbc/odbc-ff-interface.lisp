;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LispWorks, ACL and CormanLisp
;; Version 0.9
;; Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.
 
(in-package :plain-odbc)


(def-foreign-type sql-handle :pointer-void)
(def-foreign-type string-ptr (* :char))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet 
      ((define-foreign-function (name args ret)
           (list 'def-function name 
                 (mapcar 
                  (lambda (x) (list (first x) (second x))) 
                  args)
                 :returning ret :module "odbc32.dll")))
           
    (define-foreign-function "SQLAllocEnv"
        ((*phenv (* sql-handle))    ; HENV   FAR *phenv
         )
      :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLAllocConnect"
    ((henv sql-handle)          ; HENV        henv
     (*phdbc (* sql-handle))    ; HDBC   FAR *phdbc
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLConnect"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (*szDSN :cstring)        ; UCHAR  FAR *szDSN
     (cbDSN :short)             ; SWORD       cbDSN
     (*szUID :cstring)        ; UCHAR  FAR *szUID 
     (cbUID :short)             ; SWORD       cbUID
     (*szAuthStr :cstring)    ; UCHAR  FAR *szAuthStr
     (cbAuthStr :short)         ; SWORD       cbAuthStr
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDriverConnect"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (hwnd sql-handle)          ; SQLHWND     hwnd
     (*szConnStrIn :cstring)  ; UCHAR  FAR *szConnStrIn
     (cbConnStrIn :short)       ; SWORD       cbConnStrIn
     (*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
     (cbConnStrOutMax :short)   ; SWORD       cbConnStrOutMax
     (*pcbConnStrOut :pointer-void)      ; SWORD  FAR *pcbConnStrOut
     (fDriverCompletion :short) ; UWORD       fDriverCompletion
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDisconnect"
    ((hdbc sql-handle))         ; HDBC        hdbc
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLAllocStmt"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (*phstmt (* sql-handle))   ; HSTMT  FAR *phstmt
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLGetInfo"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fInfoType :short)         ; UWORD       fInfoType
     (rgbInfoValue :pointer-void)        ; PTR         rgbInfoValue
     (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
     (*pcbInfoValue :pointer-void)       ; SWORD  FAR *pcbInfoValue
     )
    :short)              ; RETCODE_SQL_API

  #+pcl
  (define-foreign-function ("SQLGetInfo" SQLGetInfo-Str)
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fInfoType :short)         ; UWORD       fInfoType
     (rgbInfoValue string-ptr)        ; PTR         rgbInfoValue
     (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
     (*pcbInfoValue :pointer-void)       ; SWORD  FAR *pcbInfoValue
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLPrepare"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr :cstring)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :long)           ; SDWORD      cbSqlStr
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLExecute"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLExecDirect"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr :cstring)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :long)           ; SDWORD      cbSqlStr
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLFreeStmt"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (fOption :short))          ; UWORD       fOption
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLCancel"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLError"
    ((henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
     (*pfNativeError :pointer-void)      ; SDWORD FAR *pfNativeError
     (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
     (cbErrorMsgMax :short)     ; SWORD       cbErrorMsgMax
     (*pcbErrorMsg :pointer-void)        ; SWORD  FAR *pcbErrorMsg
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLNumResultCols"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*pccol :pointer-void)              ; SWORD  FAR *pccol
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLRowCount"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*pcrow :pointer-void)              ; SDWORD FAR *pcrow
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDescribeCol"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (*szColName string-ptr)    ; UCHAR  FAR *szColName
     (cbColNameMax :short)      ; SWORD       cbColNameMax
     (*pcbColName (* :short))         ; SWORD  FAR *pcbColName
     (*pfSqlType (* :short))          ; SWORD  FAR *pfSqlType
     (*pcbColDef (* :unsigned-short))          ; UDWORD FAR *pcbColDef
     (*pibScale (* :short))           ; SWORD  FAR *pibScale
     (*pfNullable (* :short))         ; SWORD  FAR *pfNullable
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLColAttributes"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fDescType :short)         ; UWORD       fDescType
     (rgbDesc :pointer-void)             ; PTR         rgbDesc
     (cbDescMax :short)         ; SWORD       cbDescMax
     (*pcbDesc :pointer-void)            ; SWORD  FAR *pcbDesc
     (*pfDesc :pointer-void)             ; SDWORD FAR *pfDesc
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLColumns"
    ((hstmt sql-handle)             ; HSTMT       hstmt
     (*szTableQualifier string-ptr) ; UCHAR  FAR *szTableQualifier
     (cbTableQualifier :short)      ; SWORD       cbTableQualifier
     (*szTableOwner string-ptr)     ; UCHAR  FAR *szTableOwner
     (cbTableOwner :short)          ; SWORD       cbTableOwner
     (*szTableName string-ptr)      ; UCHAR  FAR *szTableName
     (cbTableName :short)           ; SWORD       cbTableName
     (*szColumnName string-ptr)     ; UCHAR  FAR *szColumnName
     (cbColumnName :short)          ; SWORD       cbColumnName
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLBindCol"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue :pointer-void)           ; SDWORD FAR *pcbValue
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLFetch"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
    :short)              ; RETCODE_SQL_API
    
  (define-foreign-function "SQLTransact"
    ((henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (fType :short)             ; UWORD       fType ($SQL_COMMIT or $SQL_ROLLBACK)
     )
    :short)              ; RETCODE_SQL_API

  ;; ODBC 2.0
  (define-foreign-function "SQLDescribeParam"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (*pfSqlType :pointer-void)          ; SWORD  FAR *pfSqlType
     (*pcbColDef :pointer-void)          ; UDWORD FAR *pcbColDef
     (*pibScale :pointer-void)           ; SWORD  FAR *pibScale
     (*pfNullable :pointer-void)         ; SWORD  FAR *pfNullable
     )
    :short)              ; RETCODE_SQL_API
  
  ;; ODBC 2.0
  (define-foreign-function "SQLBindParameter"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (fParamType :short)        ; SWORD       fParamType
     (fCType :short)            ; SWORD       fCType
     (fSqlType :short)          ; SWORD       fSqlType
     (cbColDef :long)           ; UDWORD      cbColDef
     (ibScale :short)           ; SWORD       ibScale
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue :pointer-void)           ; SDWORD FAR *pcbValue
     )
    :short)              ; RETCODE_SQL_API
  
  ;; level 1
  (define-foreign-function "SQLGetData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue :pointer-void)           ; SDWORD FAR *pcbValue
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLParamData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*prgbValue :pointer-void)          ; PTR    FAR *prgbValue
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLPutData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValue :long)            ; SDWORD      cbValue
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLGetConnectOption"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :pointer-void)             ; PTR         pvParam
     )
    :short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLSetConnectOption"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (vParam :long)             ; UDWORD      vParam
     )
    :short)              ; RETCODE_SQL_API

;;; rav, 11.6.2005
;   SQLRETURN SQLSetConnectAttr(
;   SQLHDBC     ConnectionHandle,
;   SQLINTEGER     Attribute,
;   SQLPOINTER     ValuePtr,
;   SQLINTEGER     StringLength);
;  ValuePtr 
; [Input]
; Pointer to the value to be associated with Attribute. Depending on the value of 
; Attribute, ValuePtr will be a 32-bit unsigned integer value or will point to a 
; null-terminated character string. Note that if the Attribute argument is a 
; driver-specific value, the value in ValuePtr may be a signed integer. 

(define-foreign-function ("SQLSetConnectAttr" SQLSetConnectAttr_long)
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :long)             ; UDWORD      vParam
     (stringlength :long)
     )

  :short)              ; RETCODE_SQL_API


(define-foreign-function ("SQLSetConnectAttr" SQLSetConnectAttr_string)
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :cstring)             ; UDWORD      vParam
     (stringlength :long)
     )
  :short)              ; RETCODE_SQL_API

(define-foreign-function "SQLGetConnectAttr"
    ((HDBC sql-handle)
     (attr :long)
     (valptr :pointer-void)
     (buffer-length :long)
     (string-length :pointer-void))
  :short)

  

  (define-foreign-function "SQLSetPos"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (irow :short)              ; UWORD       irow
     (fOption :short)           ; UWORD       fOption
     (fLock :short)             ; UWORD       fLock
     )
    :short)              ; RETCODE_SQL_API

  ; level 2
  (define-foreign-function "SQLExtendedFetch"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (fFetchType :short)        ; UWORD       fFetchType
     (irow :long)               ; SDWORD      irow
     (*pcrow :pointer-void)              ; UDWORD FAR *pcrow
     (*rgfRowStatus :pointer-void)       ; UWORD  FAR *rgfRowStatus
     )
    :short)              ; RETCODE_SQL_API

  #-pcl
  (define-foreign-function "SQLDataSources"
    ((henv sql-handle)          ; HENV        henv
     (fDirection :short)
     (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
     (cbDSNMax :short)          ; SWORD       cbDSNMax
     (*pcbDSN :pointer-void)             ; SWORD      *pcbDSN
     (*szDescription string-ptr) ; UCHAR     *szDescription
     (cbDescriptionMax :short)  ; SWORD       cbDescriptionMax
     (*pcbDescription :pointer-void)     ; SWORD      *pcbDescription
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLFreeEnv"
    ((henv sql-handle)          ; HSTMT       hstmt
     )
    :short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLMoreResults"
      ((hstmt sql-handle))
    :short)))


  ;;; foreign type definitions

  (def-struct sql-c-time 
    (hour   :short)
    (minute :short)
    (second :short))
  
  (def-struct sql-c-date
    (year  :short)
    (month :short)
    (day   :short))
  
  (def-struct sql-c-timestamp
    (year     :short)
    (month    :short)
    (day      :short)
    (hour     :short)
    (minute   :short)
    (second   :short)
    (fraction :long))

(defun %put-sql-c-date (adr year month day)
  (with-cast-pointer (ptr adr 'sql-c-date)
    (setf (get-slot-value ptr 'sql-c-date 'year) year)
    (setf (get-slot-value ptr 'sql-c-date 'month) month)
    (setf (get-slot-value ptr 'sql-c-date 'day) day)))

(defun %put-sql-c-timestamp (adr year month day hour minute second fraction)
  (with-cast-pointer (ptr adr 'sql-c-timestamp)
    (setf (get-slot-value ptr 'sql-c-timestamp 'year) year)
    (setf (get-slot-value ptr 'sql-c-timestamp 'month) month)
    (setf (get-slot-value ptr 'sql-c-timestamp 'day) day)
    (setf (get-slot-value ptr 'sql-c-timestamp 'hour) hour)
    (setf (get-slot-value ptr 'sql-c-timestamp 'minute) minute)
    (setf (get-slot-value ptr 'sql-c-timestamp 'second) second)
    (setf (get-slot-value ptr 'sql-c-timestamp 'fraction) fraction)))
    

(defun timestamp-to-universal-time (adr)
  (with-cast-pointer (ptr adr 'sql-c-timestamp)
    (values
     (encode-universal-time
      (get-slot-value ptr 'sql-c-timestamp 'second)
      (get-slot-value ptr 'sql-c-timestamp 'minute)
      (get-slot-value ptr 'sql-c-timestamp 'hour)
      (get-slot-value ptr 'sql-c-timestamp 'day)
      (get-slot-value ptr 'sql-c-timestamp 'month)
      (get-slot-value ptr 'sql-c-timestamp 'year))
     (get-slot-value ptr 'sql-c-timestamp 'fraction))))
  

(defun date-to-universal-time (adr)
  (with-cast-pointer (ptr adr 'sql-c-timestamp)
    (values
     (encode-universal-time
      0 0 0
      (get-slot-value ptr 'sql-c-date 'day) 
      (get-slot-value ptr 'sql-c-date 'month)
      (get-slot-value ptr 'sql-c-date 'year)))))

(defmacro %sql-len-data-at-exec (length) 
  `(- $SQL_LEN_DATA_AT_EXEC_OFFSET ,length))
