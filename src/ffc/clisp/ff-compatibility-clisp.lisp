;;; -*- Mode: lisp -*-
;; ffc module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

;; this is the adpation of the ffc module to clisp

(in-package :ffc)

(defun ptr-add (ptr offset)
  (ffi:unsigned-foreign-address (+ (ffi:foreign-address-unsigned ptr) offset)))


(defun peek (adr type)
  (ffi:with-c-var (place 'ffi:c-pointer adr)
    (ffi:cast place `(ffi:c-ptr ,type))))

(defun poke (adr type value)
  (ffi:with-c-var (place 'ffi:c-pointer adr)
    (setf (ffi:cast place `(ffi:c-ptr ,type)) value)))


(defun peek-byte (adr)  
  (peek adr 'ffi:uint8)) 

(defun poke-byte (adr b)
  (poke adr 'ffi:uint8 b))

(defun peek-bytes (where size)
  (ffc::peek where (list 'ffi:c-array 'ffi:uint8 size)))

(defun peek-characters (adr len)
  (ffi:with-c-var (place 'ffi:c-pointer adr)
    (ffi:cast place `(ffi:c-ptr (ffi:c-array ffi:character ,len)))))

(defun poke-characters (adr str)
  (ffi:with-c-var (place 'ffi:c-pointer adr)
    (setf (ffi:cast place `(ffi:c-ptr (ffi:c-array ffi:character ,(length str)))) str)))


(defun malloc (size)
  (ffi:foreign-address 
   (ffi:allocate-shallow 'ffi:uint8 
                         ;; clisp complains if size is 0
                         :count (if (zerop size) 1 size))))

(defun poke-bytes (where a)
  (dotimes (i (length a))
    (poke-byte (ptr-add where i) (aref a i))))

(defun vector->string (v)
  (map 'string #'code-char v))

(defun string->vector (v)
  (map 'vector #'char-code v))

;;; rav, 2005-6-12

;(EXT:CONVERT-STRING-FROM-BYTES byte-vector encoding &KEY :START :END)
;converts the subsequence of byte-vector from start to end to a STRING, according to the given encoding, and returns the resulting string.

;(EXT:CONVERT-STRING-TO-BYTES string encoding &KEY :START :END)
;converts the subsequence of string from start to end to a (VECTOR (UNSIGNED-BYTE 8)), according to the given encoding, and returns the resulting byte vector.


(defun string-to-wchar-bytes (string)
  (EXT:CONVERT-STRING-TO-BYTES string charset:UNICODE-16-LITTLE-ENDIAN))

#-ignore
(defun wchar-bytes-to-string (byte-vector)
  (EXT:CONVERT-STRING-FROM-BYTES byte-vector charset:UNICODE-16-LITTLE-ENDIAN))

;; fixme, this is awork around for a bug in clisp conversions of strings of different char with
;; really
#+ignore
(defun wchar-bytes-to-string (byte-vector)
  (let ((res (make-string (truncate (length byte-vector) 2) :initial-element (code-char 1000))))
    (dotimes (i (truncate (length byte-vector) 2) res)
      (setf (char res i)
              (code-char (+ (aref byte-vector (* 2 i)) (* (aref byte-vector (+ (* 2 i) 1)) 256)))))))




(defun string-to-char* (str &optional ptr)
  (let ((ptr (or ptr (malloc (1+ (length str))))))
    (poke-bytes ptr (string->vector str))
    (poke-byte (ptr-add ptr (length str)) 0)   ; safety
    ptr))

(defun char*-to-string (ptr)
  (vector->string (peek-bytes ptr (foreign-strlen ptr))))

(defun %get-string (ptr len)
  (peek-characters ptr len))
;; old version
;;  (vector->string (peek-bytes ptr len))

;(defun %get-string (ptr len)
;  (vector->string (peek-bytes ptr len)))

  

'(defun %get-cstring (ptr)
  (char*-to-string ptr))

(defun %get-cstring (ptr &optional (start 0))
  (ffi:with-c-var (place 'ffi:c-pointer (ptr-add ptr start))
    (ffi:cast place `ffi:c-string)))

(defun  foreign-strlen (ptr)
  (do ((i 0 (1+ i)))
      ((= 0 (peek-byte (ptr-add ptr i))) i)))

(defun %get-cstring-length (ptr)
   (foreign-strlen ptr))

; this methods causes coredumps, not immediately but at some later gc
#+ignore
(defun %put-str (ptr str &optional max-length)
  (declare (ignore max-length))
  (ffi:with-c-var (place 'ffi:c-pointer  ptr)
    (setf (ffi:cast place `ffi:c-string) str)))

(defun %put-str (ptr str &optional len)
  (declare (ignore len))
  (poke-characters ptr str))

(defun %cstring-into-vector (ptr vector offset size-in-bytes)
  "Copy C string into Lisp vector."
  
  (dotimes (i size-in-bytes)
    (setf (aref vector offset)
            (code-char (peek-byte (ptr-add  ptr i))))
    (incf offset))
  offset)

(defun %cstring-into-string (ptr string offset size-in-bytes)
   (%cstring-into-vector ptr string offset size-in-bytes))

(defun %new-ptr (type &optional bytecount)
  (ffi:foreign-address 
   (malloc (or bytecount (ffi:sizeof (canonical-to-clisp-type type))))))

(defun %dispose-ptr (p)
  (ffi:foreign-free p :full nil))

(defun free (p)
  (%dispose-ptr p))

(defun %new-cstring (size)
   (allocate-dynamic-string size))


(defun %get-binary (ptr len)
  (peek-bytes ptr len))


(defun %get-unicode-string (ptr len)
  (wchar-bytes-to-string (%get-binary ptr len)))

(defun %put-binary (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (ffc::poke ptr (list 'ffi:c-array 'ffi:uint8 (length vector)) vector)
  nil)

(defun %put-unicode-string (ptr string)
  (%put-binary ptr (string-to-wchar-bytes string)))


(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defmacro %with-sql-pointer ((pointer-var) &body body)
    (let ((obj (gensym "fobj")))
    `(ffi:with-foreign-object (,obj 'ffi:c-pointer)
      (let ((,pointer-var (ffi:foreign-address ,obj)))
        (progn ,@body))))))

(defmacro %with-temporary-allocation (bindings &body body)
  (let ((alloc-types ())
        (free-types ()))
    (dolist (binding bindings)
      (case (cadr binding)
        (:string 
          (push (list (car binding)
                  (list 'allocate-dynamic-string (caddr binding))) alloc-types)
          (push (list 'free (car binding)) free-types))
	
        (otherwise  
          (push 
            (list (car binding)
                  (list 'malloc 
                        (ffi:sizeof  
                         (canonical-to-clisp-type (cadr binding))))) alloc-types)
          (push (list 'free (car binding)) free-types))))
       
    `(let ,alloc-types
	 (unwind-protect
	     (progn ,@body)
           ,@free-types))))

 
(defmacro with-cstr ((ptr str) &body body)
   `(ffi:with-foreign-string (,ptr ,(gensym) ,(gensym) ,str :null-terminated-p t)
        (progn ,@body)))

'(defmacro with-cstr ((ptr str) &body body)
   `(let ((,ptr (string-to-char* ,str)))
       (unwind-protect
        (progn ,@body)
        (free ,ptr))))


(defun %ptr-eql (ptr1 ptr2)
  (equalp ptr1 ptr2)) ;; ??

(defvar *null-pointer* nil)

(defun %null-ptr ()
  (if (and *null-pointer* (FFI:VALIDP *null-pointer*))
    *null-pointer*
    (setf *null-pointer* (ffi:unsigned-foreign-address 0))))

(defun %get-ptr (adr)
  (peek adr 'ffi:c-pointer))

(defun %get-short (adr)
  (peek adr 'ffi:short))

(defun %get-long (adr)
  (peek adr 'ffi:long))

(defun %put-long (adr long) 
  (poke adr 'ffi:long long))

(defun %get-signed-word (ptr)
  (%get-short ptr))


; should be unsigned short
(defun %get-word (ptr)
  (peek ptr 'ffi:ushort))

(defun %put-word (ptr word)
  (poke ptr 'ffi:ushort word))


(defun %get-unsigned-long (adr)
  (peek adr 'ffi:ulong))

(defun %put-unsigned-long (adr ulong) 
  (poke adr 'ffi:ulong ulong))

(defun %get-signed-long (ptr)
  (%get-long ptr))

(defun %get-single-float (adr)
  (peek adr 'ffi:single-float))

(defun %get-double-float (adr)
  (peek adr 'ffi:double-float ))

(defun %put-single-float (adr val)
  (poke adr 'ffi:single-float val))

(defun %put-double-float (adr val)
  (poke adr 'ffi:double-float val))


(defun allocate-dynamic-string (size)
  ;; fixme, RAV unicode
  (malloc (+ 1 size))
  ;(let ((str (make-string size :initial-element #\Space)))
  ;     (string-to-char* str))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonical-to-clisp-type (type)
    (case type
      ((:signed-short) 'ffi:short)
     ((string-ptr) 'ffi:c-pointer)
     ((:ptr sql-handle-ptr sql-handle) 'ffi:c-pointer)
     ((:short) 'ffi:short)
     ((:long) 'ffi:long)
     ((:double) 'ffi:double-float)
     ((:float) 'ffi:single-float)
     ((:unsigned-long 'ffi:long))
     (otherwise type))))
     ;(otherwise (error "not a known type: ~s" type)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-foreign-function (c-name args result-type 
                                            &key lisp-name documentation module)
    
    (declare (ignorable documentation module))
    (let* ((lisp-name (or lisp-name (intern (string-upcase c-name))))
           (type-list
             (mapcar (lambda (var+type)
                       (let ((type (cadr var+type)))
                         (list (car var+type)
                               (canonical-to-clisp-type type)
                               )))
                     args)))
      
      `(progn
        (ffi:def-call-out
            ,lisp-name
            (:name ,c-name) ;; ,lisp-name
	   (:arguments
	    ,@type-list)
           (:language :STDC-STDCALL )
	   (:return-type ,(canonical-to-clisp-type result-type))
           (:library #.common-lisp-user::*odbc-library-file* ))))))


(defmacro define-foreign-type (name &rest slots)
  `(ffi:def-c-struct 
    ,name
    ,@(mapcar #'(lambda (slot) 
                  (let ((slotname (first slot))
                      (slottype (second slot)))
                    (list slotname (canonical-to-clisp-type slottype))))
              slots)))

(defmacro foreign-slot (ptr type slot)
  (let ((place (gensym "place")))
    `(ffi:with-c-var (,place 'ffi:c-pointer ,ptr)
      (ffi:slot (ffi:deref (ffi:cast ,place '(ffi:c-ptr ,type))) ',slot))))

(defmacro make-record (type)
  `(%new-ptr ',type))
