;;; -*- Mode: lisp -*-

(in-package :plain-odbc)


(defun get-string (ptr length)
  (cffi:foreign-string-to-lisp ptr length nil))

(defun get-string-nts (ptr)
  (cffi:foreign-string-to-lisp ptr MOST-POSITIVE-FIXNUM  t))

(defun put-string (ptr vector)
  (cffi:lisp-string-to-foreign vector ptr (1+ (length vector))))

(defun %null-ptr () 
  (cffi:null-pointer))

(defun get-byte-vector (ptr length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (i length)
        (setf (aref res i) (cffi:mem-aref ptr :uint8 i)))
    res))


(defun put-byte-vector (ptr vector)
  (dotimes (i (length vector))
      (setf (cffi:mem-aref ptr :uint8 i) (aref vector i))))

  
(defun wchar-bytes-to-string (byte-vector)
  (let ((res (make-string (truncate (length byte-vector) 2) :initial-element (code-char 1000))))
    (dotimes (i (truncate (length byte-vector) 2) res)
      (setf (char res i)
              (code-char (+ (aref byte-vector (* 2 i)) (* (aref byte-vector (+ (* 2 i) 1)) 256)))))))

(defun string-to-wchar-bytes (string)
  (let ((vec (make-array (* 2 (length string)) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string))
      (let ((k (char-code (char string i))))
        (setf (aref vec (* 2 i)) (logand k 255)
              (aref vec (1+ (* 2 i))) (ash k -8))))
    vec))

(defun %put-unicode-string (ptr string)
  (put-byte-vector ptr (string-to-wchar-bytes string)))


(defun %get-unicode-string (ptr len)
  (wchar-bytes-to-string (get-byte-vector ptr len)))





(defun alloc-chars (size)
  (cffi:foreign-alloc :char :count (1+ size)))
   

;(defun make-unicode-string (length)
;  #+clisp
;  (make-string length)
;  #+allegro
;  (make-string length )
;  #+lispworks
;  (make-string length :initial-element (code-char 300)))
