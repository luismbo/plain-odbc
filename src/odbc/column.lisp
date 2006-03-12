;;; -*- Mode: lisp -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :plain-odbc)

(defclass column ()
  (
   ;(lisp-type :initarg :lisp-type)
   (c-type)
   (position :initarg :position)
   (hstmt :accessor hstmt :initarg :hstmt)
   (sql-type :initarg :sql-type)
   (column-name :initarg :column-name)
   (column-size :initform 0 :initarg :column-size)
   (decimal-digits :initform 0 :initarg :decimal-digits)
   (value-ptr :initform nil)
   (buffer-length :initform 0)
   (ind-ptr :initform nil :initarg :ind-ptr)
   (nullable :initarg :nullable)
   (bound :initform t)))

(defgeneric initialize-column (param arglist))

(defgeneric get-column-value (parameter))

(defun column-info-to-class-and-args (sql-type column-size decimal-digits)
  (declare (ignore decimal-digits column-size))
  (case sql-type
    ((#.$SQL_FLOAT #.$SQL_DOUBLE #.$SQL_REAL) (values 'double-column nil))
    ((#.$SQL_BINARY #.$SQL_VARBINARY)
      (values 'binary-column nil))
    ((#.$SQL_LONGVARBINARY)
      (values 'blob-column nil))
    ((#.$SQL_CHAR #.$SQL_VARCHAR) 
      (values 'string-column nil))
    ((#.$SQL_WCHAR #.$SQL_WVARCHAR)
      (values 'unicode-string-column nil))
    ((#.$SQL_LONGVARCHAR )  ; -10 ntext on sql server
      (values 'clob-column nil))
    ((#.$SQL_WLONGVARCHAR)
      (values 'uclob-column))
    ((#.$SQL_INTEGER #.$SQL_SMALLINT #.$SQL_TINYINT #.$SQL_BIT)
      (values 'integer-column nil))
    ((#.$SQL_TIMESTAMP #.$SQL_DATE)(values 'date-column nil))
    (#.$SQL_BIGINT (values 'bigint-column))
    ((-11)  ; uniqueidentifier, guid on sql server
      (values 'binary-column nil))
    ((#.$SQL_NUMERIC #.$SQL_DECIMAL) (values 'double-column nil))
    (otherwise (values 'string-column nil))))

(defun create-column (hstmt pos)
  (multiple-value-bind (column-name sql-type column-size decimal-digits nullable)
      ;; in odbc, columns are 1 based, in lisp they are 0 based
      (%describe-column hstmt (+ pos 1))
    (multiple-value-bind (column-class args)
        (column-info-to-class-and-args sql-type column-size decimal-digits)
      (let ((column (make-instance column-class
                                   :column-name column-name
                                   :position pos
                                   :hstmt hstmt
                                   :sql-type sql-type
                                   :column-size column-size
                                   :decimal-digits decimal-digits
                                        ;:ind-ptr (%new-ptr :long)
                                   :nullable nullable)))
        (initialize-column column args)
        (when (slot-value column 'bound)
          (with-slots (buffer-length value-ptr ind-ptr c-type)
              column
            ;(setf value-ptr (cffi:foreign-alloc :long buffer-length))
            (setf ind-ptr (cffi:foreign-alloc :long))
            (%bind-column hstmt 
                          pos
                          c-type
                          value-ptr
                          buffer-length
                          ind-ptr)))
          column))))

;(defun get-column-value-or-null (column)
  

;;;----------------
;;; string-column 
;;;----------------
(defclass string-column (column) ())

(defmethod initialize-column ((column string-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  (setf (slot-value column 'buffer-length)
          (if (zerop (slot-value column 'column-size))
            *max-precision*
            (if (> (slot-value column 'column-size) *max-precision*)
              (error "column ~A has length ~A, larger than maximum size ~A" 
                     (let ((name (slot-value column 'column-name)))
                       (if (equalp name "") 
                         (slot-value column 'position)
                         name))
                     (slot-value column 'column-size)
                     *max-precision*)
              (1+ (slot-value column 'column-size)))))
  (setf (slot-value column 'value-ptr) 
          (cffi:foreign-alloc :char :count (slot-value column 'buffer-length))))

(defmethod get-column-value ((column string-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
        (get-string (slot-value column 'value-ptr) len))))) 
;;;-------------------
;;;   unicode-string
;;;------------------- 

;; a simple 16 bit unicode column, in ODBC this is SQL_WCHAR (SQL_WVARCHAR) 
;; and SQL_C_WCHAR 

(defclass unicode-string-column (column) ())

(defmethod initialize-column ((column unicode-string-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_WCHAR)
  (setf (slot-value column 'buffer-length)
          ;; column-size is size in bytes, not in characters
          (if (zerop (slot-value column 'column-size))
            *max-precision*
            (if (> (slot-value column 'column-size) *max-precision*)
              (error "column ~A has length ~A, larger than maximum size ~A" 
                     (let ((name (slot-value column 'column-name)))
                       (if (equalp name "") 
                         (slot-value column 'position)
                         name))
                     (slot-value column 'column-size)
                     *max-precision*)
              (* 2 (1+ (slot-value column 'column-size))))))
  (setf (slot-value column 'value-ptr) 
          (cffi:foreign-alloc :uchar :count (slot-value column 'buffer-length))))

(defmethod get-column-value ((column unicode-string-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    ;; len is size in bytes, not characters!
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
;        (break)
        (wchar-bytes-to-string (get-byte-vector (slot-value column 'value-ptr) len))))))



;;;--------------------
;;; integer column
;;;--------------------

(defclass integer-column (column) ())

(defmethod initialize-column ((column integer-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_SLONG)
  (setf (slot-value column 'buffer-length) 
          (cffi:foreign-type-size :long))
  (setf (slot-value column 'value-ptr) 
          (cffi:foreign-alloc :long)))


(defmethod get-column-value ((column integer-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA)
      nil
      (cffi:mem-ref (slot-value column 'value-ptr) :long))))


;;;--------------------
;;; double column
;;;--------------------

(defclass double-column (column) ())

(defmethod initialize-column ((column double-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_DOUBLE)
  (setf (slot-value column 'buffer-length) 
          (cffi:foreign-type-size :double))
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :double)))

(defmethod get-column-value ((column double-column))
  ;(%get-long (slot-value column 'ind-ptr))
  ;(%get-double-float (slot-value column 'value-ptr))
   (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
     (if (= len $SQL_NULL_DATA)
       nil
       (progn
         (cffi:mem-ref (slot-value column 'value-ptr) :double)))))

;;;------------------------
;;; date column
;;;------------------------
(defclass date-column (column) ())

(defmethod initialize-column ((column date-column) args)
  (declare (ignore args)) 
  (setf (slot-value column 'c-type) $SQL_C_TIMESTAMP)
  (setf (slot-value column 'buffer-length) 
          (cffi:foreign-type-size 'sql-c-timestamp))
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :uchar :count 32)))

(defmethod get-column-value ((column date-column))
   (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA)
      nil
      (funcall *universal-time-to-date-dataype*
               (timestamp-to-universal-time (slot-value column 'value-ptr))))))

;;;--------------------------
;;; binary column
;;;--------------------------

(defclass binary-column (column) ())

(defmethod initialize-column ((column binary-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_BINARY)
  (setf (slot-value column 'buffer-length) 
          (if (zerop (slot-value column 'column-size))
            *max-precision*
            (if (> (slot-value column 'column-size) *max-precision*)
              (error "column ~A is to large" (slot-value column 'column-size))
              (slot-value column 'column-size))))
  (setf (slot-value column 'value-ptr)
          (cffi:foreign-alloc :uchar 
                              :count (slot-value column 'buffer-length))))


(defmethod get-column-value ((column binary-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA) 
      nil
      (get-byte-vector (slot-value column 'value-ptr) len))))

;;;----------------------------
;;; bigint column
;;;----------------------------
(defclass bigint-column (column) ())

(defmethod initialize-column ((column bigint-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  ;; bigint is 64 bit, 2^64 has 20 digits, additional 1 sign =21 chars, 
  ;; say 25 for safety
  (setf (slot-value column 'buffer-length) 25)
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :uchar :count 25)))

(defmethod get-column-value ((column bigint-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA) 
      nil
      (parse-integer (get-string (slot-value column 'value-ptr) len)))))

;;;----------------------------
;;; decimal column
;;;----------------------------


;; there are two versions
;; map a decimal to sql-c-char
;; map a decimal to the decimal struct, retrive the bytes and
;; build a rational
;; problems: with oracle, the string use the decimal delimiter of the nls
;;  setting. So this is not stable.
;;    
;;   with decimal, sql server cuts off the fractional part
;;     and oracle returns a decimal for columns, but if there is
;;       a computation in the select it returns a double!!!
;;        oracle odbc driver
;;   conclusion: map decimal/numeric to double and if there is need
;;        convert a column by hand to a string which has hopefully the right
;;        decimal delimiter
;;    

(defclass decimal-column (column) ())

#+ignore
(defmethod initialize-column ((column decimal-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  ;; oracle numbers have up to 37 digits, so to be on the save side say 50
  (setf (slot-value column 'buffer-length) 50))

#+ignore
(defmethod get-column-value ((column decimal-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA) 
      nil
      (get-string (slot-value column 'value-ptr) len))))



(defmethod initialize-column ((column decimal-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_NUMERIC)
;  (setf (slot-value column 'precision) (first args))
;  (setf (slot-value column 'scale) (second args))
  ;; oracle numbers have up to 37 digits, so to be on the save side say 50
  (setf (slot-value column 'buffer-length) 50))

(defmethod get-column-value ((column decimal-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA) 
      nil
      (let ((bytes (get-byte-vector (slot-value column 'value-ptr) len))
            (sum 0))
        (dotimes (i 16)
          (setf sum (+ (* 256 sum) (aref bytes (- (+ 3 16) 1 i)))))
        (* 
         sum
         (if (zerop (aref bytes 2)) -1 1) ;sign
         (expt 10 (- (aref bytes 1))))))))



;;;-----------------------------
;;; clob column
;;;-----------------------------
(defclass clob-column (column) ())

(defmethod initialize-column ((column clob-column) args)
  (declare (ignore args))
  (setf (slot-value column 'bound) nil)
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  (setf (slot-value column 'buffer-length) *max-precision*))

(defmethod get-column-value ((column clob-column))
  (let* ((value-ptr (cffi:foreign-alloc :char 
                                        :count (slot-value column 'buffer-length)))
         (ind-ptr (cffi:foreign-alloc :long)))
    (unwind-protect
      (get-character-data 
       (slot-value column 'hstmt)
       (slot-value column 'position)
       value-ptr 
       (slot-value column 'buffer-length)
       ind-ptr)
      (cffi:foreign-free value-ptr)
      (cffi:foreign-free ind-ptr))))

;;;-----------------------------
;;; uclob column
;;;-----------------------------
(defclass uclob-column (column) ())

(defmethod initialize-column ((column uclob-column) args)
  (declare (ignore args))
  (setf (slot-value column 'bound) nil)
  (setf (slot-value column 'c-type) $SQL_C_WCHAR)
  (setf (slot-value column 'buffer-length) *max-precision*))

(defmethod get-column-value ((column uclob-column))
  (let* ((value-ptr (cffi:foreign-alloc :char :count (slot-value column 'buffer-length)))
         (ind-ptr (cffi:foreign-alloc :long)))
    (unwind-protect
      (get-unicode-character-data 
       (slot-value column 'hstmt)
       (slot-value column 'position)
       value-ptr 
       (slot-value column 'buffer-length)
       ind-ptr)
      (cffi:foreign-free value-ptr)
      (cffi:foreign-free ind-ptr))))
        
;;;-----------------------------
;;; blob column
;;;-----------------------------
(defclass blob-column (column) ())

(defmethod initialize-column ((column blob-column) args)
  (declare (ignore args))
  (setf (slot-value column 'bound) nil)
  (setf (slot-value column 'c-type) $SQL_C_BINARY)
  (setf (slot-value column 'buffer-length) *max-precision*))

(defmethod get-column-value ((column blob-column))
  (let* ((value-ptr (cffi:foreign-alloc  :uchar :count (slot-value column 'buffer-length)))
         (ind-ptr (cffi:foreign-alloc :long)))
    (unwind-protect
      (get-binary-data 
       (slot-value column 'hstmt)
       (slot-value column 'position)
       value-ptr 
       (slot-value column 'buffer-length)
       ind-ptr)
      (cffi:foreign-free value-ptr)
      (cffi:foreign-free ind-ptr))))

;;-------------------------------
;;  fetch data via SQlGetData
;; ------------------------------
(defun get-character-data (hstmt position value-ptr buffer-length ind-ptr)
  ;; local error handling, we can not use the general error handling
  ;; since this resets the sql-state
  ;; anyway the normal error handling would warn because of 
  ;; data truncation which is in this procedure not an abnormal condition

  (flet ((handle-error (code)
           (unless (or (= code $SQL_SUCCESS)
                       (= code $SQL_SUCCESS_WITH_INFO))
             (multiple-value-bind (code2 condition)
                 (error-handling-fun code nil nil hstmt)
               (declare (ignore code2))
               (error condition)))))
    (let* ((sqlret (%sql-get-data-raw hstmt
                                      position
                                      $SQL_C_CHAR ; always the same
                                      value-ptr
                                      buffer-length
                                      ind-ptr)))
      (handle-error sqlret)
      (let ((len (cffi:mem-ref ind-ptr :long)))
        ;(break)
        (cond 
          ((= len $sql_null_data) nil)
          ;; character data has a 0 byte appended, the length does not include it
          ;; but it is taken into account when placing the data into the buffer
          ((and (/= len $SQL_NO_TOTAL)
                (<= (+ 1 len) buffer-length))
            ;; the data fits into the buffer, return it
            (get-string value-ptr len))
          
          ;; we have to fetch the data in several steps
          (t 
            (let ((sos (make-string-output-stream)))
              (loop
                (if (and (= sqlret $SQL_SUCCESS_WITH_INFO)
                         (equal (sql-state nil nil hstmt)
                                "01004"))
                  ;; an 0 byte is append to a string, ignore that
                  
                  (let ((str (get-string value-ptr (1- buffer-length))))
                    (write-string str sos)
                    (setf sqlret (%sql-get-data-raw hstmt
                                            position
                                            $SQL_C_CHAR
                                            value-ptr
                                            buffer-length
                                            ind-ptr))
                    (handle-error sqlret))
                  (return)))
              ;; fetch the last part of the data
          (setf len (cffi:mem-ref ind-ptr :long))
          (let ((str (get-string value-ptr len)))
            (write-string str sos))
          (get-output-stream-string sos))))))))

;;; the version for 16bit unicode 

(defun get-unicode-character-data (hstmt position value-ptr buffer-length ind-ptr)
  ;; local error handling, we can not use the general error handling
  ;; since this resets the sql-state
  ;; anyway the normal error handling would warn because of 
  ;; data truncation which is in this procedure not an abnormal condition

  (flet ((handle-error (code)
           (unless (or (= code $SQL_SUCCESS)
                       (= code $SQL_SUCCESS_WITH_INFO))
             (multiple-value-bind (code2 condition)
                 (error-handling-fun code nil nil hstmt)
               (declare (ignore code2))
               (error condition)))))
    (let* ((sqlret (%sql-get-data-raw hstmt
                                      position
                                      $SQL_C_WCHAR ; always the same
                                      value-ptr
                                      buffer-length
                                      ind-ptr)))
      (handle-error sqlret)
      (let ((len (cffi:mem-ref ind-ptr :long)))
        (cond 
          ((= len $sql_null_data) nil)
          ;; character data has a 0 byte appended, the length does not include it
          ;; but it is taken into account when placing the data into the buffer
          ((and (/= len $SQL_NO_TOTAL)
                (<= (+ 2 len) buffer-length))
            ;; the data fits into the buffer, return it
            (%get-unicode-string value-ptr len))
          
          ;; we have to fetch the data in several steps
          (t 
            (let ((sos (make-string-output-stream :element-type 'character)))
              (loop
                (if (and (= sqlret $SQL_SUCCESS_WITH_INFO)
                         (equal (sql-state nil nil hstmt)
                                "01004"))
                  ;; an 0 byte is append to a string, ignore that
                  
                  (let ((str (%get-unicode-string value-ptr (- buffer-length 2))))
                    (write-string str sos)
                    (setf sqlret (%sql-get-data-raw hstmt
                                            position
                                            $SQL_C_WCHAR
                                            value-ptr
                                            buffer-length
                                            ind-ptr))
                    (handle-error sqlret))
                  (return)))
              ;; fetch the last part of the data
          (setf len (cffi:mem-ref ind-ptr :long))
          (let ((str (%get-unicode-string value-ptr len)))
            (write-string str sos))
          (get-output-stream-string sos))))))))
    
(defun get-binary-data (hstmt position value-ptr buffer-length ind-ptr)
  ;; local error handling, we can not use the general error handling
  ;; since this resets the sql-state
  ;; anyway the normal error handling would warn because of 
  ;; data truncation which is in this procedure not an abnormal condition
  
  (flet ((handle-error (code)
           (unless (or (= code $SQL_SUCCESS)
                       (= code $SQL_SUCCESS_WITH_INFO))
             (multiple-value-bind (code2 condition)
                 (error-handling-fun code nil nil hstmt)
               (declare (ignore code2))
               (error condition)))))
 
  (let* ((sqlret (%sql-get-data-raw hstmt
                                    position
                                    $SQL_C_BINARY
                                    value-ptr
                                    buffer-length
                                    ind-ptr)))
    (handle-error sqlret)
    (let ((len (cffi:mem-ref ind-ptr :long)))
      (if (= len $sql_null_data)
        nil
        (let ((res (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
              (res-len 0))
          (loop
            (if (and (= sqlret $SQL_SUCCESS_WITH_INFO)
                     (equal (sql-state nil nil hstmt)
                          "01004"))
            
            (let ((vec (get-byte-vector value-ptr buffer-length)))
              (setf res (adjust-array res (+ res-len buffer-length)))
              (setf (subseq res res-len (+ res-len buffer-length)) vec)
              (setf res-len (length res))
              (setf sqlret (%sql-get-data-raw hstmt
                                              position
                                              $SQL_C_BINARY
                                              value-ptr
                                              buffer-length
                                              ind-ptr))
              (handle-error sqlret))
            (return)))
        
        (setf len (cffi:mem-ref ind-ptr :long))
        (let ((vec (get-byte-vector value-ptr len)))
          (setf res (adjust-array res (+ res-len len)))
          (setf (subseq res res-len (+ res-len len)) vec))
        res))))))

