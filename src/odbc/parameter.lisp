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
            (:unicode-string 'unicode-string-parameter)
            (:integer 'integer-parameter)
            (:date 'date-parameter)
            (:binary 'binary-parameter)
            (:double 'double-parameter)
            (:clob 'clob-parameter)
            (:unicode-clob 'uclob-parameter)
            (:blob 'blob-parameter))))
    (let ((param (make-instance class-name 
                                :direction direction 
                                :lisp-type lisp-type
                                :position position
                                :query query)))
      (initialize-parameter param args)
      param)))

(defun bind-parameter (hstmt pos param)
 #+ignore  
 (setf (slot-value param 'value-ptr) 
         (%new-ptr :ptr (slot-value param 'buffer-length)))
  (setf (slot-value param 'ind-ptr) 
          (uffi:allocate-foreign-object :long))
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
    (uffi:free-foreign-object value-ptr)
    (setf value-ptr nil) 
    (uffi:free-foreign-object ind-ptr)
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
      (setf buffer-length length-of-buffer)
      (setf value-ptr (uffi:allocate-foreign-string length-of-buffer)))))

(defmethod set-parameter-value ((param string-parameter) value)
  (cond
    ((null value)
      (setf  (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
              $SQL_NULL_DATA)
      (%put-str (slot-value param 'value-ptr) "" 0))
    (t 
      (%put-str (slot-value param 'value-ptr) value (length value))
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
              (length  value)))))

(defmethod get-parameter-value ((param string-parameter))
  (let ((len (uffi:deref-pointer (slot-value param 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
        (%get-string (slot-value param 'value-ptr) len)))))

;;------------------------
;; unicode-string parameter
;;------------------------
(defclass unicode-string-parameter (direct-parameter)
  ())

(defmethod initialize-parameter ((param unicode-string-parameter) args)
  (let ((length-of-buffer (* 2 (or (car args) *default-string-parameter-size*))))
    (with-slots (value-type parameter-type buffer-length 
                            column-size value-ptr
                            ind-ptr) param
      (setf value-type $SQL_C_WCHAR)
      (setf parameter-type $SQL_WVARCHAR)
      (setf column-size length-of-buffer)
      (setf buffer-length length-of-buffer)
      (setf value-ptr (uffi:allocate-foreign-object :unsigned-byte length-of-buffer)))))

(defmethod set-parameter-value ((param unicode-string-parameter) value)
  (cond
    ((null value)
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long) 
              $SQL_NULL_DATA)
      ;; not necessary
      (%put-unicode-string (slot-value param 'value-ptr) ""))
    (t 
      (%put-unicode-string (slot-value param 'value-ptr) value)
      (setf  (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
              (* 2 (length  value))))))

(defmethod get-parameter-value ((param unicode-string-parameter))
  (let ((len (uffi:deref-pointer (slot-value param 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
        (%get-unicode-string (slot-value param 'value-ptr) len))))) 

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
    (setf buffer-length 4)
    (setf value-ptr (uffi:allocate-foreign-object :long))))

(defmethod set-parameter-value ((param integer-parameter) value)
  (cond 
    ((null value)
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
              $SQL_NULL_DATA))
    (t (setf (uffi:deref-pointer (slot-value param 'value-ptr) :long) 
               value)
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long) 0))))

(defmethod get-parameter-value ((param integer-parameter))
  (let ((len (uffi:deref-pointer (slot-value param 'ind-ptr) :long)))
    (if (= len $SQL_NULL_DATA)
      nil
      (progn
        (uffi:deref-pointer (slot-value param 'value-ptr) :long)))))


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
     (setf buffer-length 8)
     (setf value-ptr (uffi:allocate-foreign-object :double ))))

(defmethod set-parameter-value ((param double-parameter) value)
   (cond  
     ((null value)
       (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long) 
               $SQL_NULL_DATA))
     (t 
       (setf (uffi:deref-pointer (slot-value param 'value-ptr) :double)
               (coerce value 'double-float))
       (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long) 8))))

(defmethod get-parameter-value ((param double-parameter))
    (if (= (uffi:deref-pointer (slot-value param 'ind-ptr) :long) $SQL_NULL_DATA)
      nil
      (uffi:deref-pointer (slot-value param 'value-ptr) :double)))

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
     (setf buffer-length 24)
     (setf value-ptr (uffi:allocate-foreign-object :unsigned-byte 24))))


(defmethod set-parameter-value ((param date-parameter) value)
  (if (null value)
    (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
            $SQL_NULL_DATA)
    (progn
      ;; fixme warum 1?
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long) 1)
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time  
           (funcall *date-datatype-to-universal-time* value))
        (%put-sql-c-timestamp (slot-value param 'value-ptr) year month day hour min sec 0)))))

(defmethod get-parameter-value ((param date-parameter))
  (let ((len (uffi:deref-pointer (slot-value param 'ind-ptr) :long)))
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
  (let ((length-of-buffer (or (car args) *default-binary-parameter-size* )))
    (with-slots (value-type parameter-type buffer-length value-ptr
                            ind-ptr) param
      (setf value-type $SQL_C_BINARY)
      (setf parameter-type $SQL_VARBINARY)
      (setf buffer-length length-of-buffer)
      (setf value-ptr (uffi:allocate-foreign-object :unsigned-byte length-of-buffer)))))

(defmethod set-parameter-value ((param binary-parameter) value)
(if (null value)
  (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
          $SQL_NULL_DATA)
    (if (< (slot-value param 'buffer-length) (length value))
      (progn
        (error "buffer is to small")
        ; we could increase the buffer size with another bind parameter
        ; or set data_at_execution =1
        )
      (progn
        ;(break)
        (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long) 
                (length value))
        (%put-binary (slot-value param 'value-ptr) value)))))


(defmethod get-parameter-value ((param binary-parameter))
  (let ((len (uffi:deref-pointer (slot-value param 'ind-ptr) :long)))
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
    (setf buffer-length 4)
    (setf value-ptr (uffi:allocate-foreign-object :long))))

(defmethod set-parameter-value ((param clob-parameter) value)
  (if (null value)
    (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
            $SQL_NULL_DATA)
    (progn
      (setf (slot-value param 'temp-val) value)
      (setf (uffi:deref-pointer (slot-value param 'value-ptr) :long) 
                                (slot-value param 'position))
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
              (%sql-len-data-at-exec (length value))))))

(defmethod send-parameter-data ((param clob-parameter) hstmt)
  (let* ((temp-val (slot-value param 'temp-val))
         (value-len (length temp-val))
         (buffer-length (min *lob-fetch-buffer-size* value-len))
         ;; fixme charcater length and UTF8
         ;; fixme buffer-length +1 since %put-str adds a trailing zero byte
         (buffer (uffi:allocate-foreign-string  (+ buffer-length 1))))
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
      (uffi:free-foreign-object buffer)
      )))

;;;--------------------
;;; uclob parameter
;;;--------------------

(defclass uclob-parameter (lob-parameter) ())


(defmethod initialize-parameter ((param uclob-parameter) args)
  (declare (ignore args))
  (with-slots (value-type parameter-type buffer-length value-ptr
                          ind-ptr) param
    (setf value-type $SQL_C_WCHAR)
    (setf parameter-type $SQL_WLONGVARCHAR)
    ;; the value-ptr will be needed to find the parameter,  
    ;; we store the position there
    (setf buffer-length 4)
    (setf value-ptr (uffi:allocate-foreign-object :long))))

(defmethod set-parameter-value ((param uclob-parameter) value)
  (if (null value)
    (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
            $SQL_NULL_DATA)
    (progn
      (setf (slot-value param 'temp-val) value)
      (setf (uffi:deref-pointer (slot-value param 'value-ptr) :long)
              (slot-value param 'position))
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
                 (%sql-len-data-at-exec (* 2 (length value)))))))

(defmethod send-parameter-data ((param uclob-parameter) hstmt)
  (let* ((temp-val (slot-value param 'temp-val))
         (value-len (length temp-val))
         (buffer-length-in-chars (min (truncate *lob-fetch-buffer-size* 2) value-len))
         (buffer (uffi:allocate-foreign-object :unsigned-byte (* 2 (+ buffer-length-in-chars 1)))))
    (let ((pos 0))
      (loop
        (let ((len (min (- value-len pos) buffer-length-in-chars)))
          (%put-unicode-string buffer 
                    (subseq temp-val pos (+ pos len)))
          (let ((res (%sql-put-data hstmt buffer (* 2 len))))
            
            (declare (ignore res))
            (setf pos (+ pos len))
            (if (>= pos value-len)
              (return)))))
      (uffi:free-foreign-object buffer)
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
    (setf buffer-length 4)
    (setf value-ptr (uffi:allocate-foreign-object :long))))

(defmethod set-parameter-value ((param blob-parameter) value)
  (if (null value)
    (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
            $SQL_NULL_DATA)
    (progn
      (setf (slot-value param 'temp-val) value)
      (setf (uffi:deref-pointer (slot-value param 'value-ptr) :long)
              (slot-value param 'position))
      (setf (uffi:deref-pointer (slot-value param 'ind-ptr) :long)
              (%sql-len-data-at-exec (length value))))))

(defmethod send-parameter-data ((param blob-parameter) hstmt)
  (let* ((temp-val (slot-value param 'temp-val))
         (len (length temp-val))
         (buffer (uffi:allocate-foreign-object :unsigned-byte 
                                               (if (zerop len) 1 len))))
    (%put-binary buffer
                 temp-val
                 len)
    (let ((res (%sql-put-data hstmt buffer len)))
      (declare (ignore res)))
    (uffi:free-foreign-object buffer)))

