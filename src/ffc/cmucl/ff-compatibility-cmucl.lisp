;;;-*- Mode: Lisp; Package: FFC -*-

;; Foreign function compatibility module for MCL, CMUCL, LispWorks, ACL
;; and CormanLisp (CMUCL version)
;; Version 0.5
;; Copyright (C) Paul Meurer 2000, 2001. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; In this file the platform specific code is isolated.
;; The code in this file consists mostly of wrapper functions and macros 
;; around the platform-dependent foreign function interface.

;; This file contains CMUCL specific code.

(in-package :ffc)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *foreign-module* nil))

;; CMUCL specific code

(defun %get-cstring-length (ptr)
  (loop with size = 0
        until (char= (deref ptr size) #\Null)
        do (incf size)
        finally (return size)))

(defun %cstring-into-string (ptr string offset size-in-bytes)
    (dotimes (i size-in-bytes)
      (setf (char string offset)
            (deref ptr i))
      (incf offset))
    offset)

(defun %cstring-into-vector (ptr vector offset size-in-bytes)
    (dotimes (i size-in-bytes)
      (setf (aref vector offset)
            (deref ptr i))
      (incf offset))
    offset)

(defmacro with-cstr ((ptr str) &body body)
  `(with-alien ((,ptr c-string ,str))
     ,@body))

(defun %cstring-to-keyword (pointer)
  (let* ((len (%get-cstring-length pointer))
         (str (make-string len)))
    (declare (dynamic-extent str))
    (%cstring-into-string pointer str 0 len)
    (intern str (find-package 'keyword))))

(defmacro %new-ptr (type &optional bytecount)
  (if bytecount
      `(make-alien ,type ,bytecount)
    `(make-alien ,type)))

(defun %dispose-ptr (p)
  (free-alien p))

(defmacro %with-sql-pointer ((pointer-var) &body body)
  `(let ((,pointer-var (make-alien sql-handle-ptr)))
     ,@body))

(defmacro %null-ptr ()
  '(system:int-sap 0))

(defmacro %ptr-eql (ptr1 ptr2)
  `(= (system:sap-int (alien-sap ,ptr1))
      (system:sap-int (alien-sap ,ptr2))))

(defmacro %address-to-pointer (address)
  `(system:int-sap ,address))

(defmacro %pointer-to-address (pointer)
  `(system:sap-int (alien-sap ,pointer)))

;; all the same ...

(defmacro %get-ptr (ptr) `(deref ,ptr))
(defmacro %get-short (ptr) `(deref ,ptr))
(defmacro %put-short (ptr short) `(setf (%get-ptr ,ptr) ,short))
(defmacro %get-bit (ptr) `(deref ,ptr))
(defmacro %get-long (ptr) `(deref ,ptr))
(defmacro %put-long (ptr long) `(setf (%get-ptr ,ptr) ,long))
(defmacro %get-signed-word (ptr) `(deref ,ptr))
(defmacro %get-word (ptr) `(deref ,ptr))
(defmacro %put-word (ptr word) `(setf (%get-ptr ,ptr) ,word))
(defmacro %get-unsigned-long (ptr) `(deref ,ptr))
(defmacro %get-signed-long (ptr) `(deref ,ptr))
(defmacro %get-single-float (ptr) `(deref ,ptr))
(defmacro %get-double-float (ptr) `(deref ,ptr))

(defun %get-cstring (ptr)
  (if (stringp ptr)
      ;; have to distinguish between foreign string
      ;; allocated with MAKE-ALIEN and WITH-ALIEN (which through advanced
      ;; magic behaves like an ordinary string)
      (let ((null-pos (position #\Null ptr)))
	(if null-pos
	    (subseq ptr 0 null-pos)
	  ptr))
    (cast (addr (deref ptr 0)) c-string)))

#+lispworks
(defmacro %put-str (ptr string &optional max-length)
  (let ((size (gensym)))
    `(let ((,size (length ,string)))
       (when (and ,max-length (> ,size ,max-length))
         (error "string \"~a\" of length ~d is longer than max-length: ~d"
                ,string ,size ,max-length))
       (dotimes (i ,size)
         (setf (fli:dereference ,ptr :index i) (char ,string i)))
       (setf (fli:dereference ,ptr :index ,size) 0))))

;; huh? where is it used?
(defmacro %put-str (ptr string &optional max-length)
  (declare (ignore max-length))
  (setf ptr string))

(defun %new-cstring (byte-count)
  (make-alien char byte-count))

#+lispworks
(defmacro make-record (type)
  `(fli:allocate-foreign-object :type ',type))

(defmacro make-record (type)
  `(make-alien ,type))

;(load-foreign "~/adabas/odbc/adabasodbc.so")
;; openlink lib
(load-foreign "/usr/local/lib/libodbc.so")
;(load-foreign "/opt/openlink/lib/oplodbc.so") ; .1

(def-alien-type :ptr (* t))
(def-alien-type sql-handle (* t))
(def-alien-type sql-handle-ptr (* sql-handle))
;(def-alien-type sql-handle-ptr sql-handle)
;; ??
(def-alien-type string-ptr c-string) ;; (* t))
;(def-alien-type :word (signed 32))
(def-alien-type :byte (unsigned 8))
(def-alien-type :signed-short (signed 16))
(def-alien-type :short (signed 16))
(def-alien-type :unsigned-short (unsigned 16)) 
(def-alien-type :signed-long (signed 32))
(def-alien-type :unsigned-long (unsigned 32))
(def-alien-type :long (signed 32))
(def-alien-type :double double-float)
(def-alien-type :boolean (boolean 8)) ;; *** is size right?

(defmacro define-foreign-function (c-name-or-list args result-type
						  &key documentation module)
  (declare (ignore documentation module))
  (let* ((c-name (if (consp c-name-or-list)
		    (car c-name-or-list)
		   c-name-or-list))
	 (name (intern (string-upcase c-name))))
    `(declaim (inline ,name))
    `(def-alien-routine ,c-name-or-list ,result-type
       ,@args)))

(defmacro %with-temporary-allocation (bindings &body body)
  (let ((alien-bindings ())
        (let-bindings ())) 
    (dolist (binding bindings)
      (case (cadr binding)
        (:string
	 (push (list (car binding)
		     `(make-string ,(caddr binding)
				   :element-type 'base-char
				   :initial-element #\Null))
	       let-bindings))
        (otherwise
	 (push (list (car binding) (cadr binding)) alien-bindings)
	 ;; we have to reference the alien vars to conform with the other
	 ;; FFIs and loose the elegance of CMUCL's FFI
	 (push (list (car binding) `(addr ,(car binding))) let-bindings))))
    `(with-alien ,alien-bindings
       (let ,let-bindings
         ,@body))))

;; returns a byte array
#+lispworks
(defun %get-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (ecase format
    (:unsigned-byte-vector ; thanks for fix to Marc Battyani
     (let ((vector (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len)
         (setf (aref vector i)
	       (fli:dereference ptr :index i :type :unsigned-byte)))
       vector))
    (:bit-vector
     (let ((vector (make-array (ash len 3) :element-type 'bit)))
       (dotimes (i len)
         (let ((byte (fli:dereference ptr :index i :type :unsigned-byte)))
           (dotimes (j 8)
             (setf (bit vector (+ (ash i 3) j))
		   (logand (ash byte (- j 7)) 1)))))
       vector))))

;; returns size in bytes
#+lispworks
(defun %put-binary (ptr vector &optional max-length)
  (cond ((bit-vector-p vector)
         (let* ((bit-count (length vector))
                (byte-count (print (ceiling bit-count 8))))
           (when (and max-length (> byte-count max-length))
             (error "bit vector of length ~d is longer than max-length: ~d"
                    bit-count (* max-length 4)))
           (dotimes (i byte-count)
             (let ((byte 0))
               (dotimes (j 8)
                 (let ((index (+ (ash i 3) j)))
                   (if (< index bit-count)
                       (setf byte (logior byte
					  (ash (bit vector (+ (ash i 3) j))
					       (- 7 j))))
                     (return))))
               (setf (fli:dereference ptr :index i :type :unsigned-byte) byte)))
           byte-count))
        (t (error "not yet implemented"))))

#+lispworks
(defmacro %new-binary (byte-count)
  `(fli:allocate-foreign-object :type :unsigned-byte :nelems ,byte-count))

;; returns a byte array
(defun %get-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (ecase format
    (:unsigned-byte-vector ; thanks for fix to Marc Battyani
     (let ((vector (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len)
         (setf (aref vector i) (deref ptr i)))
       vector))
    (:bit-vector
     (let ((vector (make-array (ash len 3) :element-type 'bit)))
       (dotimes (i len)
         (let ((byte (deref ptr i)))
           (dotimes (j 8)
             (setf (bit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))))
       vector))))

;; returns size in bytes
(defun %put-binary (ptr vector &optional max-length)
  (cond ((bit-vector-p vector)
         (let* ((bit-count (length vector))
                (byte-count (print (ceiling bit-count 8))))
           (when (and max-length (> byte-count max-length))
             (error "bit vector of length ~d is longer than max-length: ~d"
                    bit-count (* max-length 4)))
           (dotimes (i byte-count)
             (let ((byte 0))
               (dotimes (j 8)
                 (let ((index (+ (ash i 3) j)))
                   (if (< index bit-count)
                       (setf byte (logior byte (ash (bit vector
							 (+ (ash i 3) j))
						    (- 7 j))))
                     (return))))
               (setf (deref ptr i) byte)))
           byte-count))
        (t (error "not yet implemented"))))

(defmacro %new-binary (byte-count)
  `(make-alien :unsigned-byte ,byte-count))

(defmacro define-foreign-type (name &rest slots)
  `(alien:def-alien-type
    ,name
    (alien::struct
     ,name
     ,@slots)))

(defmacro foreign-slot (ptr type slot)
  (declare (ignore type))
  `(alien:slot ,ptr ',slot))
