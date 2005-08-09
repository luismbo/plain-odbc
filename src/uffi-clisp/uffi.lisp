;;;; Mini-UFFI for CLISP
;;;; enough to get some CL-SQL running (postgres, odbc)
;;;; (c) 2004-2005 Joerg Hoehle
;;;; Copyright: Allegro's LispLGPL (LLGPL)

(defpackage "UFFI"
  (:use "FFI" "COMMON-LISP")
  (:shadow "WITH-FOREIGN-OBJECT")
  (:export
   "DEF-TYPE" "DEF-FOREIGN-TYPE" "DEF-ENUM" "DEF-UNION"
   "DEF-STRUCT" "GET-SLOT-VALUE" "GET-SLOT-POINTER"
   "WITH-FOREIGN-OBJECT" "WITH-FOREIGN-OBJECTS"
   "ALLOCATE-FOREIGN-OBJECT" "FREE-FOREIGN-OBJECT"
   "DEREF-POINTER" "DEREF-ARRAY"
   "WITH-CAST-POINTER"
   "MAKE-NULL-POINTER" "NULL-POINTER-P"
   "MAKE-POINTER" "POINTER-ADDRESS"
   "ALLOCATE-FOREIGN-STRING" "CONVERT-FROM-FOREIGN-STRING"
   "FOREIGN-STRING-LENGTH"
   "WITH-CSTRING" "WITH-CSTRINGS" "FREE-CSTRING"
   "CONVERT-FROM-CSTRING" "CONVERT-TO-CSTRING"
   "ENSURE-CHAR-CHARACTER" "ENSURE-CHAR-INTEGER" "ENSURE-CHAR-STORABLE"
   "CHAR-ARRAY-TO-POINTER"
   "FIND-FOREIGN-LIBRARY" "LOAD-FOREIGN-LIBRARY"
   "DEFAULT-FOREIGN-LIBRARY-TYPE"
   "DEF-FUNCTION"))

(in-package "UFFI")

#|
;;;; Principles of the conversion from UFFI to CLISP FFI

o The lowest level is used when interfacing to function, which means that
  all that is known are either scalar types, arrays, opaque or typed
  pointers (FFI:C-POINTER). There's no :out or :malloc-free left at all.

o Always expand UFFI:def* to FFI:def* so as not to duplicate what the
  FFI:def* macros do.
Corollary: We need a mapping from UFFI to FFI's external representation.

o Foreign objects are mostly represented by CLISP FOREIGN-VARIABLE
  objects, i.e. typed pointers. Pointers emanate from:
  a) uffi:allocate-foreign-object
  b) with-foreign-object
  c) function :return-type
  d) struct slots
Luckily, both a) and b) create FOREIGN-VARIABLE OBJECTS. c) will
probably cause some headaches.

o Using FOREIGN-VARIABLE will allow to use FFI:ELEMENT, CAST etc. -- it is
  the only way to get array/memory access with the CLISP FFI.

o :cstring looks like it can be mapped to ffi:c-string, i.e. be kept
  as a Lisp string.

o (:array x) probably is best rewritten as (* x)

o All places call convert-uffi-type at compile- (macroexpansion-)time
  and insert literal result (like def-struct does)
    (uffi:allocate '(* foo))
 -> (ffi:allocate-shallow 'ffi:c-pointer)
 -> (ffi:foreign-allocate (parse-c-type 'ffi:c-pointer))
 -> (ffi:foreign-allocate 'ffi:c-pointer) (compiler macro)
 instead of
    (ffi:foreign-allocate (parse-c-type (convert-uffi-type '(* foo))))

TODO
- array of undeclared size
- with-cast-pointer does not provide for array bounds
- bug in odbc-api.lisp:get-cast-binary:
  cast to :byte yet uses (unsigned-byte 8) vector


o Even when types are evaluated (like uffi:allocate-* says it does), restrict
  to the constant forms '(* ...) or :keyword
  -- "Types are known at compile time."
  Maybe otherwise emit a warning and generate run-time convert-uffi-type form.

o Pros and cons of *uffi-types* table:
- def-foreign-type, def-struct, def-enum must maintain it and cannot only
  expand to their FFI counterpart.
+ may permit UFFI-specific error messages?
...

;; I lost track about which version of CLISP <2.33 might work with this.
;; This package makes use of (c-pointer foo) types, foreign-variable
;; constructors and other things I added to the CLISP FFI around 2003-2004.

|#

(defvar *uffi-internal-types* (make-hash-table :test #'equal :size 30))
(defvar *uffi-types*          (make-hash-table :test #'equal :size 30))

(dolist (entry
	 '((:void nil)			;only as :return-type
	   (:pointer-void   c-pointer)
	   (:char           character)	;not to be confused with ffi:char
	   (:unsigned-char  character)
	   (:cstring        c-string)
	   (:byte  sint8)		;UFFI mandates 8/16/32 bits
	   (:short sint16)		;despite the generic names
	   (:int   sint32)
	   (:long  long)
	   (:unsigned-byte  uint8)
	   (:unsigned-short uint16)
	   (:unsigned-int   uint32)
	   (:unsigned-long  ulong)
	   (:float  single-float)
	   (:double double-float)))
  (setf (gethash (first entry) *uffi-internal-types*)
	(ffi:parse-c-type (second entry)))
  (setf (gethash (first entry) *uffi-types*)
	(second entry)))
;*uffi-internal-types*

(defun convert-uffi-internal-type (utype &optional place)
  (declare (ignore place))
  (or (gethash utype *uffi-internal-types*)
      (error "Unknown UFFI Type ~S" utype)))
;; The internal-type table can be considered as a leftover from the time where
;; I thought that efficiency would be gained by having UFFI types convert to
;; FFI's internal structure, in effect caching ffi:parse-c-type.
;; Since that time, forms like uffi:allocate-foreign-object have been changed
;; to use regular FFI types via ffi:allocate-shallow and convert-uffi-type.

;; This converts to the documented CLISP FFI types instead
;; of the internal c-type representation.
(defun convert-uffi-type (type &optional place)
  ;;(declare place (member nil function return type ffi:c-struct))
  (etypecase type
    (CONS
     (ecase (first type)
       ((*)
	;;TODO? for some cases use `(ffi:c-pointer ,c-type)
         (list 'ffi:c-pointer (convert-uffi-type (second type))))
;;    rav, replaced with above
;;       'ffi:c-pointer)
       ((:array)
	(destructuring-bind (c-array elt &optional size) type
	  (declare (ignore c-array))
	  (if size `(ffi:c-array ,(convert-uffi-type elt place) ,size)
	      ;; else same as (* type)
	      'ffi:c-pointer)))
       ((:struct-pointer)
	`(ffi:c-pointer ,(convert-uffi-type (second type) place)))
       ((:struct)
	(convert-uffi-type (second type) place))))
    (SYMBOL
     (multiple-value-bind (c-type found)
	 (gethash type *uffi-types*)
       (unless found (error "Unknown UFFI type ~S" type))
       ;; need to have things turned to pointers for functions
       (if (and (member place '(function return) :test #'eq)
		;; consp is too gross, but good enough
		;; since (c-array #) will not be a funarg in UFFI
		;;TODO bug: the internal type is not list-based
		(consp (gethash type *uffi-internal-types*)))
	   'ffi:c-pointer c-type)))))

(defun convert-uffi-type-form (form &key value (if-not-exists :error))
  ;; To use with forms that evaluate arguments
  (assert (eq if-not-exists :error))
  ;;TODO handle if-not-exists - how?
  (flet ((convert (form)
	   (etypecase form
	     ((cons (eql quote) (cons * null))
	      (values (convert-uffi-type (second form) 'type) t))
	     ((and symbol (satisfies constantp)) ;encompasses keyword
	      ;;TODO? &whole env (constantp form env)
	      (values (convert-uffi-type (symbol-value form) 'type) t)))))
    (let ((ctype (convert form)))
      ;;TODO if-not-exists & value error
      ;;TODO if-not-exists warn "run-time-overhead"
      ;; but generate (parse-c-type (convert-uffi-type x))
      (if value ctype `',ctype))))

(defmacro def-foreign-type (name type)
  ;; Type is *not* evaluated, but checked at compile-time
  (check-type name symbol)
  (let ()
    ;; 1. def-c-type name c-type
    ;; 2. update *uffi-types*
    ;; 3. update *uffi-internal-types*
    ;; TODO use def-c-type directly instead of duplicating it
    `(eval-when (load compile eval)
      (setf (gethash ',name *uffi-internal-types*)
       (ffi:parse-c-type
	(setf (gethash ',name *uffi-types*)
	      ',(convert-uffi-type type 'type)) ',name))
      ',name)))
;(def-foreign-type sql-handle :pointer-void)

;; ffi:def-c-struct creates a defstruct Lisp-equivalent which UFFI doesn't
;; need because the structure is never converted as a whole. Use a vector-based
;; representation of the slots instead, which can be obtained with def-c-type.
(defmacro def-struct (name &rest slots)
  `(progn
    (ffi:def-c-type ,name
      ;(ffi:c-struct vector		;suppress cl:defstruct overhead
        ;; rav, replaced vector by list
        (ffi:c-struct list
	.,(loop for (symbol type) in slots
		collect (list symbol
			      (if (eq type :pointer-self)
				  `(ffi:c-pointer ,name)
				(convert-uffi-type type 'ffi:c-struct))))))
    (eval-when (load compile eval)
      (setf (gethash ',name *uffi-types*) ',name))))

(defmacro def-union (name &rest slots)
  `(progn
    (ffi:def-c-type ,name
      (ffi:c-union
	.,(loop for (symbol type) in slots
		collect (list symbol
			      (convert-uffi-type type 'ffi:c-struct)))))
    (eval-when (load compile eval)
      (setf (gethash ',name *uffi-types*) ',name))))

(defmacro def-type (name type)
  ;; Type is not evaluated
  (convert-uffi-type type 'type)	;compile-time error if unknown
  `(deftype ,name () '(or ffi:foreign-variable ffi:foreign-address null)))

;;;; -------------------------------- Enum
(defmacro def-enum (name (&rest names))
  (check-type name symbol)
  (flet ((enum-name (elt)
	   (intern (concatenate 'string
	    (symbol-name name) "#" (symbol-name elt)))))
    `(progn
      (ffi:def-c-enum ,name
	.,(mapcar
	   (lambda (elt)
	     (etypecase elt
	       (SYMBOL
		(enum-name elt))
	       ((CONS SYMBOL (CONS INTEGER NULL))
		(list (enum-name (first elt)) (second elt)))))
	   names))
      (eval-when (load compile eval)
	(setf (gethash ',name *uffi-types*) ',name)))))

;;;; -------------------------------- Memory Access

#|
Principle: FOREIGN-VARIABLE objects are the only
ones that allow memory access in CLISP's FFI.
We assume that uffi:deref-pointer etc. is passed such an object.

Unbounded array access on untyped pointers are dealt with
 (inefficiently) in deref-array
|#

;; setf-able
(defmacro deref-pointer (ptr type)
  ;;TODO evaluate type or not -- clsql is inconsistent again
  (declare (ignore type))
  ;;(convert-uffi-type type)		;compile-time error if unknown
  ;;(convert-uffi-type-form type)		;compile-time error if unknown
  `(ffi:foreign-value ,ptr))

;;About the problem with indexing (* foo) as an array:
;; It's most efficient to use deref-array with ffi::%element and
;; therefore cast an untyped pointer to an array so large that deref-array
;; will always work. This array will never be dereferenced (i.e. converted)
;; as a whole by UFFI code, so I see no problem with doing so.
;;Wrong: with-cast pointer is also advocated with deref-pointer (not
;; encountered in clsql), i.e. an array may not be correct!
;;Since with-cast-pointer is not used everywhere around deref-array,
;; this approach would benefit from the implementation of (c-pointer <ctype>)
;; at least for :return-type in CLISP (see case c above)
(defconstant +huge-unknown-array-size+ (ash 1 31))

;;rav, this will not work with retrieving slots

;(defmacro with-cast-pointer ((binding-name ptr type) &body body)
;  `(let ((,binding-name
;	  (ffi:foreign-variable ,ptr
;	   (ffi:parse-c-type
;	    `(ffi:c-array
;	      ;;,,(convert-uffi-type-form type); worse code
;	      ,',(convert-uffi-type-form type :value t)
;	      ,+huge-unknown-array-size+))))) .,body))


(defmacro with-cast-pointer ((binding-name ptr type) &body body)
  `(let ((,binding-name
	  (ffi:foreign-variable ,ptr
	   (ffi:parse-c-type
	      ;;,,(convert-uffi-type-form type); worse code
	      ',(convert-uffi-type-form type :value t)))))
          ,@ body))


(defun %ensure-array (array type)
  ;;TODO? inline as macro so parse-c-type gets inlined
  (etypecase array
    (ffi:foreign-variable	array)
    (ffi:foreign-address
     (warn "Inefficient use of UFFI:DEREF-ARRAY")
     ;;(with-cast-pointer (the-array array type) the-array)
     (ffi:foreign-variable array
      (ffi:parse-c-type
	 `(c-array
	   ,(convert-uffi-type (second type)) ;expect (:array ...)
	   ,+huge-unknown-array-size+))))))

;; setf-able
(defmacro deref-array (array type position)
  ;; Type is evaluated
  ;; and always(?) looks like '(:array foo)
  (convert-uffi-type-form type)
  `(ffi:foreign-value (ffi::%element (%ensure-array ,array ,type) ,position)))

(defmacro get-slot-value (obj type field)
  (convert-uffi-type-form type)		;compile-time error if unknown
  `(ffi:foreign-value (ffi::%slot ,obj ,field)))
;;TODO Problem with nested structs (UFFI unclear), cf. e-mail von rif Sep.2004
;;TODO get-slot-value of pointer type yields pointer, not value, so must avoid
;; foreign-value and solely use %slot, depending on type.

(defmacro get-slot-pointer (obj type field)
  ;; When slot type is some pointer type
  ;; Specified to return object pointed to, not pointer to slot
  ;; Therefore identical to get-slot-value
  (convert-uffi-type-form type)		;compile-time error if unknown
  `(ffi:foreign-value (ffi::%slot ,obj ,field)))

;;;; -------------------------------- Allocation

(defmacro free-foreign-object (x)
  ;; We must not mark invalid because free-foreign-object may also be
  ;; called on objects returned via a function's :return-type, which
  ;; all share their foreign-base.
  `(ffi:foreign-free ,x :full nil))

;; UFFI only has shallow (calloc-like) allocations.
(defmacro allocate-foreign-object (type &optional size)
  (if size
    `(ffi:allocate-shallow ,(convert-uffi-type-form type) :count ,size)
    `(ffi:allocate-shallow ,(convert-uffi-type-form type))))
    

(defmacro with-foreign-object ((var type) &body body)
  `(ffi:with-foreign-object (,var ,(convert-uffi-type-form type)) .,body))

;; This could perhaps benefit from a single
;; (with-foreign-object (c-struct x y z) (let ((x (slot 1) y z ...))))
;; pro: one closure only, contra: complex parse-c-type -> should use
;; (load-time-value (parse-c-type #))
(defmacro with-foreign-objects ((one &rest more) &body body)
  (list* 'with-foreign-object one
	 (if more
	     (list (list* 'with-foreign-objects more body)) body)))

(defmacro allocate-foreign-string (size &key (unsigned 't))
  ;; ffi:allocate yields type (c-array-max character size)
  ;; which allows elementwise access
  `(ffi:allocate-shallow 'character :count (prog1 ,size ,unsigned)))




;;;; -------------------------------- :cstring

;; Rationale: UFFI acknowledges that it may be the Lisp string itself
;; and does not allow access to individual characters or other operations.

(defmacro with-cstring ((cstring string) &body body)
  `(let ((,cstring ,string)) .,body))
(defmacro with-cstrings ((&rest bindings) &body body)
  (reduce #'(lambda (binding form) (list 'with-cstring binding form))
	  bindings ; may be ()
	  :initial-value `(progn .,body) :from-end t))

(defun free-cstring (cstring)
  (declare (ignore cstring))
  (values))

(defmacro convert-from-cstring (cstring)
  cstring)
(defmacro convert-to-cstring (cstring)
  cstring)

;;;; -------------------------------- Libraries

(defun default-foreign-library-type ()
  ;; load-time-value has two benefits:
  ;; + the .fas file is portable
  ;; + no run-time overhead after loading
  (or
   (load-time-value
    (cond ((member :cygwin *features*) "dll") ; ??
	  ((member :win32  *features*) "dll")
	  ((member :unix   *features*) "so")))
   (error "Please configure foreign library extension")))

(defvar *module-translations*
  (list #+win32 (cons "odbc" "odbc32.dll")
	(cons nil :default))
  "Set this to an alist of (module.path) of libraries to use.")
;(setq uffi::*module-translations* ())
;(push (cons "odbc" *odbc-lib*) uffi::*module-translations*)  ;clisp-2.28
;(push (cons "odbc" "odbc32.dll") uffi::*module-translations*);newer clisp
;(push (cons "odbc" "/usr/lib/libodbc.so.1") uffi::*module-translations*)
;(push (cons "postgresql" "/usr/lib/libpq.so.3") uffi::*module-translations*)
;(push (cons "clsql-uffi" ".../clsql-3.0.3/uffi/uffi.so") uffi::*module-translations*)
;(push (cons nil :default) uffi::*module-translations*)

(defun find-foreign-library (name-s paths &key drive-letters types)
  (declare (ignore paths drive-letters types))
  name-s)

(defun load-foreign-library (name &key module supporting-libraries)
  (declare (ignore supporting-libraries))
  ;; name is always result of (find-foreign-library) in clsql
  (unless module
    (warn "No UFFI module name for library ~A" name))
  ;; We could load the library, but here we can return T/NIL
  ;; and defer loading to def-function
  (assoc module *module-translations* :test #'equal))

;;;; -------------------------------- Functions

(defun to-lisp-name (name)
  ;; READ handles readtable-case, INTERN does not
  (read-from-string name))

;(if (find-symbol "FOREIGN-ALLOC*" "FFI") ;custom JCH-CLISP
;(defmacro def-function (name args &key module (returning ':void))
;  (check-type name (or string (cons string (cons symbol null))))
;  (multiple-value-bind (c-name lisp-name)
;      (if (consp name)
;	  (values (first name) (second name))
;	  (values name (to-lisp-name name)))
;    `(def-lib-call-out ,lisp-name
;       (cdr (assoc ',module *module-translations* :test #'equal))
;       (:name ,c-name)
;       (:arguments
;	.,(loop for (name type) in args
;	       collect (list name (convert-uffi-type type 'function))))
;       (:return-type ,(convert-uffi-type returning 'return))
;       (:language #+win32 :stdc-stdcall #-win32 :stdc))))

(defmacro def-function (name args &key module (returning ':void))
  (check-type name (or string (cons string (cons symbol null))))
  (labels ((pointer-type-p (type)
             (let ((type2 (ffi:deparse-c-type (ffi:parse-c-type type))))
               (and (consp type2)
                    (eq (car type2) 'ffi:c-pointer)))))
  (multiple-value-bind (c-name lisp-name)
      (if (consp name)
        (values (first name) (second name))
        (values name (to-lisp-name name)))
    (let ((lisp-name-internal (intern (concatenate 'string "%" 
                                                   (string lisp-name)))))
     `(progn
       (ffi:def-call-out ,lisp-name-internal
           (:name ,c-name)
         (:library (cdr (assoc ',module *module-translations* :test #'equal)))
         (:arguments
          ,@(mapcar (lambda (arg) 
                      (let ((name (first arg)) (type (second arg))) 
                  (list name 
                        (if (pointer-type-p  (convert-uffi-type type 'function))
                          'ffi:c-pointer
                          (convert-uffi-type type 'function)))))
                    args))
          (:return-type ,(convert-uffi-type returning 'return))
          (:language #+win32 :stdc-stdcall #-win32 :stdc))
       (defun ,lisp-name ,(mapcar #'first args)
         (,lisp-name-internal 
          ,@(mapcar 
             (lambda (arg)
               (if (pointer-type-p (convert-uffi-type (second arg)))
                 (list  'ffi:foreign-address (first arg))
                 (first arg)))
             args
             ))))))))
    
;)

#| Allow to load any files & static type checking
(defmacro def-function (name args &key module (returning ':void))
  (check-type name (or string (cons string (cons symbol null))))
  (multiple-value-bind (c-name lisp-name)
      (if (consp name)
	  (values (first name) (second name))
	  (values name (to-lisp-name name)))
    (unless module (warn "~A: module unknown" c-name))
   `(progn
     (ffi:def-call-out ,(gensym c-name) (:name "ffi_identity")
       (:arguments
	.,(loop for (name type) in args
	       collect (list name (convert-uffi-type type 'function))))
       (:return-type ,(convert-uffi-type returning 'return))
       (:language #+win32 :stdc-stdcall #-win32 :stdc))
     (defun ,lisp-name (&rest args)
       (error "FFI stub only: ~S" (cons ',lisp-name args))))))






|#
;;;; -------------------------------- Pointers

(if (find-symbol "FOREIGN-ALLOC*" "FFI")
(defmacro null-pointer-p (ptr)		;in CLISP <=2.30
  `(ffi:foreign-address-null ,ptr))
(defmacro null-pointer-p (ptr)		;in CLISP >2.30 (NIL/NULL conversion)
  `(null ,ptr)))
(defmacro make-null-pointer (type)	;nil<->NULL conversion in CLISP
  (convert-uffi-type-form type)		;compile-time error if unknown
  ''nil)

(defmacro make-pointer (address type)
  (convert-uffi-type type)		;compile-time error if unknown
  ;;NB this constructor does not yield NIL for 0
  `(ffi:unsigned-foreign-address ,address))
(defmacro pointer-address (ptr)
  `(ffi:foreign-address-unsigned ,ptr))



;;; rav stuff 

(defmacro def-array-pointer (name-array type)
 `(progn
   (ffi:def-c-type ,name-array (FFI:C-ARRAY ,(uffi::convert-uffi-type type :declaration),2000000 ))
   (eval-when (load compile eval)
     (setf (gethash ',name-array *uffi-types*) ',name-array))))


(defmacro deref-array (obj type i)
  (declare (ignore type)) 
  `(ffi:element (ffi:foreign-value ,obj) ,i))

(defmacro convert-to-foreign-string (obj)
  (let ((strvar (gensym "string"))) 
    `(let ((,strvar (concatenate 'string ,obj  (string (code-char 0)))))
      (FFI:FOREIGN-VARIABLE
       (FFI:ALLOCATE-DEEP 'CHARACTER ,strvar :count (length ,strvar))
       ',(ffi:parse-c-type 'ffi:character)))))
  

(defmacro convert-from-foreign-string (obj &key
					   length
					   (locale :default)
					   (null-terminated-p t))
  `(clisp-convert-from-foreign-string ,obj ,length ,null-terminated-p)
  )


(defun clisp-convert-from-foreign-string (obj length null-terminated-p)
  (declare (ignore locale))
  (if null-terminated-p 
    (ffi:foreign-value 
     (ffi:foreign-variable 
      obj (ffi:parse-c-type (list 'FFI:C-ARRAY-MAX 'character 
                                  (if length length 16000000)))))
    (ffi:foreign-value  (ffi:foreign-variable 
                         obj (ffi:parse-c-type (list  'ffi:c-array 'character length))))))


(export '(convert-to-foreign-string convert-from-foreign-string def-array-pointer deref-array ))
;;;; EoF
