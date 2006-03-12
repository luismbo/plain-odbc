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

(define-foreign-library :odbc (t (:default "odbc32"  )))
(load-foreign-library :odbc)

(defctype sql-handle :pointer)
(defctype *sql-handle :pointer)
(defctype RETCODE :short)
(defctype *short :pointer)
(defctype *sdword :pointer)
(defctype *sword :pointer)
(defctype *ulong :pointer)


(defctype string-ptr :pointer)



(defcfun "SQLAllocEnv" retcode (penv *sql-handle))

(defcfun "SQLAllocConnect" retcode 
  (henv sql-handle)          ; HENV        henv
  (*phdbc *sql-handle))    ; HDBC   FAR *phdbc
   
(defcfun "SQLConnect" retcode
  (hdbc sql-handle)          ; HDBC        hdbc
  (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
  (cbDSN :short)             ; SWORD       cbDSN
  (*szUID string-ptr)        ; UCHAR  FAR *szUID 
  (cbUID :short)             ; SWORD       cbUID
  (*szAuthStr string-ptr)    ; UCHAR  FAR *szAuthStr
  (cbAuthStr :short)         ; SWORD       cbAuthStr
  )
  
  (defcfun "SQLDriverConnect" retcode
    (hdbc sql-handle)          ; HDBC        hdbc
    (hwnd sql-handle)          ; SQLHWND     hwnd
                                        ;(*szConnStrIn string-ptr)  ; UCHAR  FAR *szConnStrIn
    (*szConnStrIn string-ptr)  ; UCHAR  FAR *szConnStrIn
    (cbConnStrIn :short)       ; SWORD       cbConnStrIn
                                        ;(*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
     (*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
     (cbConnStrOutMax :short)   ; SWORD       cbConnStrOutMaxw 
     (*pcbConnStrOut *short)      ; SWORD  FAR *pcbConnStrOut
     (fDriverCompletion :unsigned-short) ; UWORD       fDriverCompletion
     )
   
  (defcfun "SQLDisconnect" retcode
    (hdbc sql-handle))         ; HDBC        hdbc
  
  (defcfun "SQLAllocStmt" retcode 
    (hdbc sql-handle)          ; HDBC        hdbc
    (*phstmt *sql-handle))   ; HSTMT  FAR *phstmt


  
  (defcfun "SQLGetInfo" retcode
    (hdbc sql-handle)          ; HDBC        hdbc
    (fInfoType :short)         ; UWORD       fInfoType
    (rgbInfoValue :pointer)        ; PTR         rgbInfoValue
    (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
    (*pcbInfoValue :pointer)       ; SWORD  FAR *pcbInfoValue
     )

  (defcfun ("SQLGetInfo" SQLGetInfo-Str) retcode 
    (hdbc sql-handle)          ; HDBC        hdbc
     (fInfoType :short)         ; UWORD       fInfoType
     (rgbInfoValue string-ptr)        ; PTR         rgbInfoValue
     (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
     (*pcbInfoValue :pointer)       ; SWORD  FAR *pcbInfoValue
     )


  (defcfun "SQLPrepare" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr string-ptr)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :long)           ; SDWORD      cbSqlStr
     )

  
  (defcfun "SQLExecute" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     )

  
  (defcfun "SQLExecDirect" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr string-ptr)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :long)           ; SDWORD      cbSqlStr
     )

  
  (defcfun "SQLFreeStmt" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (fOption :short))          ; UWORD       fOption

  
  (defcfun "SQLCancel" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     )

  
  (defcfun "SQLError" retcode 
    (henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (hstmt sql-handle)         ; HSTMT       hstmt
;     (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
     (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
     (*pfNativeError *SDWORD)      ; SDWORD FAR *pfNativeError
;     (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
     (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
     (cbErrorMsgMax :short)     ; SWORD       cbErrorMsgMax
     (*pcbErrorMsg *short))        ; SWORD  FAR *pcbErrorMsg
  


  (defcfun "SQLNumResultCols" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (*pccol :pointer)              ; SWORD  FAR *pccol
     )

  
  (defcfun "SQLRowCount" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (*pcrow *sdword)              ; SDWORD FAR *pcrow
     )

  
  (defcfun "SQLDescribeCol" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (*szColName string-ptr)    ; UCHAR  FAR *szColName
     (cbColNameMax :short)      ; SWORD       cbColNameMax
     (*pcbColName *short)         ; SWORD  FAR *pcbColName
     (*pfSqlType *short)          ; SWORD  FAR *pfSqlType
     (*pcbColDef *ulong)          ; UDWORD FAR *pcbColDef
     (*pibScale *short)           ; SWORD  FAR *pibScale
     (*pfNullable *short)         ; SWORD  FAR *pfNullable
     )

  
  (defcfun "SQLColAttributes" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fDescType :short)         ; UWORD       fDescType
     (rgbDesc :pointer)             ; PTR         rgbDesc
     (cbDescMax :short)         ; SWORD       cbDescMax
     (*pcbDesc *sword)            ; SWORD  FAR *pcbDesc
     (*pfDesc *sdword)             ; SDWORD FAR *pfDesc
     )


  (defcfun "SQLColumns" retcode 
    (hstmt sql-handle)             ; HSTMT       hstmt
     (*szTableQualifier string-ptr) ; UCHAR  FAR *szTableQualifier
     (cbTableQualifier :short)      ; SWORD       cbTableQualifier
     (*szTableOwner string-ptr)     ; UCHAR  FAR *szTableOwner
     (cbTableOwner :short)          ; SWORD       cbTableOwner
     (*szTableName string-ptr)      ; UCHAR  FAR *szTableName
     (cbTableName :short)           ; SWORD       cbTableName
     (*szColumnName string-ptr)     ; UCHAR  FAR *szColumnName
     (cbColumnName :short)          ; SWORD       cbColumnName
     )


  (defcfun "SQLBindCol" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :pointer)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue *sdword)           ; SDWORD FAR *pcbValue
     )

  
  (defcfun "SQLFetch" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     )

    
  (defcfun "SQLTransact" retcode 
    (henv sql-handle)          ; HENV        henv
    (hdbc sql-handle)          ; HDBC        hdbc
    (fType :short)             ; UWORD       fType ($SQL_COMMIT or $SQL_ROLLBACK)
    )


  ;; ODBC 2.0
  (defcfun "SQLDescribeParam" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (*pfSqlType *sword)          ; SWORD  FAR *pfSqlType
     (*pcbColDef *ulong)          ; UDWORD FAR *pcbColDef
     (*pibScale *sword)           ; SWORD  FAR *pibScale
     (*pfNullable *sword)         ; SWORD  FAR *pfNullable
     )

  
  ;; ODBC 2.0
  (defcfun "SQLBindParameter" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (fParamType :short)        ; SWORD       fParamType
     (fCType :short)            ; SWORD       fCType
     (fSqlType :short)          ; SWORD       fSqlType
     (cbColDef :ulong)           ; UDWORD      cbColDef
     (ibScale :short)           ; SWORD       ibScale
     (rgbValue :pointer)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue *sdword)           ; SDWORD FAR *pcbValue
     )

  
  ;; level 1
  (defcfun "SQLGetData" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :pointer)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue *sdword)           ; SDWORD FAR *pcbValue
     )


  (defcfun "SQLParamData" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (*prgbValue :pointer)          ; PTR    FAR *prgbValue
     )

  
  (defcfun "SQLPutData" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (rgbValue :pointer)            ; PTR         rgbValue
     (cbValue :long)            ; SDWORD      cbValue
     )

  
  (defcfun "SQLGetConnectOption" retcode 
    (hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :pointer)             ; PTR         pvParam
     )

  
  (defcfun "SQLSetConnectOption" retcode 
    (hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (vParam :ulong)             ; UDWORD      vParam
     )


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

(defcfun ("SQLSetConnectAttr" SQLSetConnectAttr_long) retcode 
    (hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :long)             ; UDWORD      vParam
     (stringlength :long)
     )


(defcfun ("SQLSetConnectAttr" SQLSetConnectAttr_string) retcode
    (hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam string-ptr)             ; UDWORD      vParam
     (stringlength :long)
     )

  

  (defcfun "SQLSetPos" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (irow :short)              ; UWORD       irow
     (fOption :short)           ; UWORD       fOption
     (fLock :short)             ; UWORD       fLock
     )


  ; level 2
  (defcfun "SQLExtendedFetch" retcode 
    (hstmt sql-handle)         ; HSTMT       hstmt
     (fFetchType :short)        ; UWORD       fFetchType
     (irow :long)               ; SDWORD      irow
     (*pcrow :pointer)              ; UDWORD FAR *pcrow
     (*rgfRowStatus :pointer)       ; UWORD  FAR *rgfRowStatus
     )

  (defcfun "SQLDataSources" retcode 
    (henv sql-handle)          ; HENV        henv
     (fDirection :short)
     (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
     (cbDSNMax :short)          ; SWORD       cbDSNMax
     (*pcbDSN *sword)             ; SWORD      *pcbDSN
     (*szDescription string-ptr) ; UCHAR     *szDescription
     (cbDescriptionMax :short)  ; SWORD       cbDescriptionMax
     (*pcbDescription *sword)     ; SWORD      *pcbDescription
     )


  (defcfun "SQLFreeEnv" retcode 
    (henv sql-handle)          ; HSTMT       hstmt
    )


  (defcfun "SQLMoreResults" retcode 
      (hstmt sql-handle))


  ;;; foreign type definitions

  (defcstruct sql-c-time ""
    (hour   :short)
    (minute :short)
    (second :short))
  
  (defcstruct sql-c-date ""
    (year  :short)
    (month :short)
    (day   :short)) 
  
  (defcstruct sql-c-timestamp ""
    (year     :short)
    (month    :short)
    (day      :short)
    (hour     :short)
    (minute   :short)
    (second   :short)
    (fraction :long))

(defun %put-sql-c-date (adr %year %month %day)
  (setf (foreign-slot-value adr 'sql-c-date 'year) %year)
  (setf (foreign-slot-value adr 'sql-c-date 'month) %month)
  (setf (foreign-slot-value adr 'sql-c-date 'day) %day))

 
(defun %put-sql-c-timestamp (adr %year %month %day %hour %minute %second %fraction)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'second) %second)
  (setf (foreign-slot-value adr 'sql-c-timestamp  'minute) %minute)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'hour) %hour)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'day) %day)
  (setf (foreign-slot-value adr 'sql-c-timestamp  'month) %month)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'year) %year)
  (setf (foreign-slot-value adr 'sql-c-timestamp 'fraction) %fraction)
  )    

(defun timestamp-to-universal-time (adr)
  (with-foreign-slots 
      ((year month day hour minute second fraction) adr sql-c-timestamp)
    (values
     (encode-universal-time
      second
      minute 
      hour 
      day 
      month 
      year )
     fraction)))
    

(defun date-to-universal-time (adr)
  (with-foreign-slots 
      ((year month day) adr sql-c-date)
    (encode-universal-time
      0 0 0
      day
      month
      year)))


(defmacro %sql-len-data-at-exec (length) 
  `(- $SQL_LEN_DATA_AT_EXEC_OFFSET ,length))
