;;; -*- Mode: Lisp -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :plain-odbc)

(defclass parameter ()
  ((query :initarg :query :reader query)
   (lisp-type :initarg :lisp-type)
   (position :initarg :position)
   (value-type)
   (parameter-type)
   (direction :initarg :direction)
   (column-size :initform 0)
   (decimal-digits :initform 0)
   (ind-ptr :initform nil)
   (value-ptr :initform nil)
   (buffer-length :initform 0)))

(defclass direct-parameter (parameter)
 ())

(defgeneric initialize-parameter (param arglist))

(defgeneric set-parameter-value (parameter value))

(defgeneric get-parameter-value (parameter))

(defgeneric free-parameter (parameter))


; position is needed for data at exec time
(defun create-parameter (query position lisp-type direction args)
  (let ((class-name
          (ecase lisp-type 
            (:string 'string-parameter)
            (:integer 'integer-parameter)
            (:date 'date-parameter)
            (:binary 'binary-parameter)
            (:double 'double-parameter)
            (:clob 'clob-parameter)
            (:blob 'blob-parameter))))
    (let ((param (make-instance class-name 
                                :direction direction 
                                :lisp-type lisp-type
                                :position position
                                :query query)))
      (initialize-parameter param args)
      param)))

(defun bind-parameter (hstmt pos param)
  (setf (slot-value param 'value-ptr) 
          (%new-ptr :ptr (slot-value param 'buffer-length)))
  (setf (slot-value param 'ind-ptr) 
          (%new-ptr :long))
  (%sql-bind-parameter 
   hstmt
   pos
   (ecase (slot-value param 'direction)
     (:in $SQL_PARAM_INPUT)
     (:out $SQL_PARAM_OUTPUT)
     (:inout $SQL_PARAM_INPUT_OUTPUT))
   (slot-value param 'value-type)
   (slot-value param 'parameter-type)
   (slot-value param 'column-size)
   (slot-value param 'decimal-digits)
   (slot-value param 'value-ptr)
   (slot-value param 'buffer-length)
   (slot-value param 'ind-ptr)))


(defmethod free-parameter ((param parameter))
  (with-slots (value-ptr ind-ptr) param
    (%dispose-ptr value-ptr)
    (setf value-ptr nil) 
    (%dispose-ptr ind-ptr)
    (setf ind-ptr nil)))

(defun free-parameters (query)
  (with-slots (parameters) query
    (when (and (slot-boundp query 'parameters)
               (slot-value query 'parameters))
      (dotimes (i (length parameters))
        (free-parameter (aref parameters i)))
      (setf parameters nil))))



;;------------------------
;; string parameter
;;------------------------
(defclass string-parameter (direct-parameter)
  ())

(defmethod initialize-parameter ((param string-parameter) args)
  (let ((length-of-buffer (or (car args) *default-string-parameter-size*)))
    (with-slots (value-type parameter-type buffer-length 
                            column-size value-ptr
                            ind-ptr) param
      (setf value-type $SQL_C_CHAR)
      (setf parameter-type $SQL_VARCHAR)
      (setf column-size length-of-buffer)
      (setf buffer-length length-of-buffer))))

(defmethod set-parameter-value ((param string-parameter) value)
  (cond
    ((null value)
      (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA)
      (%put-str (slot-value param 'value-ptr) "" 0))
    (t 
      (%put-str (slot-value param 'value-ptr) value (length value))
      (%put-long (slot-value param 'ind-ptr) (length  value)))))

(defmethod get-parameter-value ((param string-parameter))
  (let ((len (%get-long (slot-value param 'ind-ptr))))
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
        (%get-string (slot-value param 'value-ptr) len)))))

;;----------------------
;; integer parameter
;;----------------------

(defclass integer-parameter (direct-parameter) 
  ())

(defmethod initialize-parameter ((param integer-parameter) args)
  (assert (not args))
  (with-slots (value-type parameter-type buffer-length value-ptr
                          ind-ptr) param
    (setf value-type $SQL_C_LONG)
    (setf parameter-type $SQL_INTEGER)
    (setf buffer-length 4)))

(defmethod set-parameter-value ((param integer-parameter) value)
  (cond 
    ((null value)
      (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA))
    (t (%put-long (slot-value param 'value-ptr) value)
      (%put-long (slot-value param 'ind-ptr) 0))))

(defmethod get-parameter-value ((param integer-parameter))
  (let ((len (%get-long (slot-value param 'ind-ptr))))
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
        (%get-long (slot-value param 'value-ptr))))))


;;----------------------------
;; double parameter
;;----------------------------

(defclass double-parameter (direct-parameter)
  ())

(defmethod initialize-parameter ((param double-parameter) args)
  (assert (not args))
   (with-slots (value-type parameter-type buffer-length value-ptr
                           ind-ptr) param
     (setf value-type $SQL_C_DOUBLE)
     (setf parameter-type $SQL_DOUBLE)
     (setf buffer-length 8)))

(defmethod set-parameter-value ((param double-parameter) value)
   (cond  
     ((null value)
       (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA))
     (t (%put-double-float (slot-value param 'value-ptr) value)
       (%put-long (slot-value param 'ind-ptr) 8))))

(defmethod get-parameter-value ((param double-parameter))
    (if (= (%get-long (slot-value param 'ind-ptr)) $SQL_NULL_DATA)
      nil
      (%get-double-float (slot-value param 'value-ptr))))

;;-----------------
;; date parameter
;;-----------------

(defclass date-parameter (direct-parameter)
  ())

(defmethod initialize-parameter ((param date-parameter) args)
  (assert (not args))
   (with-slots (value-type parameter-type buffer-length value-ptr
                           ind-ptr) param
     (setf value-type $SQL_C_TIMESTAMP)
     (setf parameter-type $SQL_TIMESTAMP)
     ;;fixme length
     (setf buffer-length 24)))


(defmethod set-parameter-value ((param date-parameter) value)
  (if (null value)
    (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA)
    (progn
      ;; fixme warum 1?
      (%put-long (slot-value param 'ind-ptr) 1)
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time  
           (funcall *date-datatype-to-universal-time* value))
        (%put-sql-c-timestamp (slot-value param 'value-ptr) year month day hour min sec 0)))))

(defmethod get-parameter-value ((param date-parameter))
  (let ((len (%get-long (slot-value param 'ind-ptr))))
    (if (= len $SQL_NULL_DATA) 
      nil
      (funcall *universal-time-to-date-dataype*
                (timestamp-to-universal-time (slot-value param 'value-ptr))))))


;;------------------
;; binary parameter
;;------------------

(defclass binary-parameter (direct-parameter)
  ())

(defmethod initialize-parameter ((param binary-parameter) args)
  (let ((length-of-buffer (or *default-binary-parameter-size* (car args))))
    (with-slots (value-type parameter-type buffer-length value-ptr
                            ind-ptr) param
      (setf value-type $SQL_C_BINARY)
      (setf parameter-type $SQL_VARBINARY)
      (setf buffer-length length-of-buffer))))

(defmethod set-parameter-value ((param binary-parameter) value)
(if (null value)
  (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA)
    (if (< (slot-value param 'buffer-length) (length value))
      (progn
        (error "buffer is to small")
        ; we could increase the buffer size with another bind parameter
        ; or set data_at_execution =1
        )
      (progn
        (%put-long (slot-value param 'ind-ptr) (length value))
        (%put-binary (slot-value param 'value-ptr) value)))))


(defmethod get-parameter-value ((param binary-parameter))
  (let ((len (%get-long (slot-value param 'ind-ptr))))
    (if (= len $SQL_NULL_DATA) 
      nil
      (%get-binary (slot-value param 'value-ptr) len))))


;;;-----------------------
;;;    clob parameter
;;;-----------------------

(defclass lob-parameter (parameter) (temp-val))

(defgeneric send-parameter-data (param hstmt))


(defclass clob-parameter (lob-parameter) ())


(defmethod initialize-parameter ((param clob-parameter) args)
  (declare (ignore args))
  (with-slots (value-type parameter-type buffer-length value-ptr
                          ind-ptr) param
    (setf value-type $SQL_C_CHAR)
    (setf parameter-type $SQL_LONGVARCHAR)
    ;; the value-ptr will be needed to find the parameter,  
    ;; we store the position there
    (setf buffer-length 4)))

(defmethod set-parameter-value ((param clob-parameter) value)
  (if (null value)
    (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA)
    (progn
      (setf (slot-value param 'temp-val) value)
      (%put-long (slot-value param 'value-ptr) (slot-value param 'position))
      (%put-long (slot-value param 'ind-ptr)
                 (%sql-len-data-at-exec (length value))))))

(defmethod send-parameter-data ((param clob-parameter) hstmt)
  (let* ((temp-val (slot-value param 'temp-val))
         (value-len (length temp-val))
         (buffer-length (min *lob-fetch-buffer-size* value-len))
         ;; fixme charcater length and UTF8
         ;; fixme buffer-length +1 since %put-str adds a trailing zero byte
         (buffer (%new-ptr :ptr (+ buffer-length 1))))
    (let ((pos 0))
      (loop
        (let ((len (min (- value-len pos) buffer-length)))
          (%put-str buffer 
                    (subseq temp-val pos (+ pos len))
                    len)
          (let ((res (%sql-put-data hstmt buffer len)))
            (declare (ignore res))
            (setf pos (+ pos len))
            (if (>= pos value-len)
              (return)))))
      (%dispose-ptr buffer)
      )))


;;;----------------------
;;; blob-parameter
;;;----------------------


(defclass blob-parameter (lob-parameter) ())

(defmethod initialize-parameter ((param blob-parameter) args)
  (declare (ignore args))
  (with-slots (value-type parameter-type buffer-length value-ptr
                          ind-ptr) param
    (setf value-type $SQL_C_BINARY)
    (setf parameter-type $SQL_LONGVARBINARY)
    ;; the value-ptr will be needed to find the parameter, 
    ;; we store the position there
    (setf buffer-length 4)))

(defmethod set-parameter-value ((param blob-parameter) value)
  (if (null value)
    (%put-long (slot-value param 'ind-ptr) $SQL_NULL_DATA)
    (progn
      (setf (slot-value param 'temp-val) value)
      (%put-long (slot-value param 'value-ptr) (slot-value param 'position))
      (%put-long (slot-value param 'ind-ptr)
                 (%sql-len-data-at-exec (length value))))))

(defmethod send-parameter-data ((param blob-parameter) hstmt)
  (let* ((temp-val (slot-value param 'temp-val))
         (len (length temp-val))
         (buffer (%new-ptr :ptr len)))
    (%put-binary buffer
                 temp-val
                 len)
    (let ((res (%sql-put-data hstmt buffer len)))
      (declare (ignore res)))
    (%dispose-ptr buffer)))

