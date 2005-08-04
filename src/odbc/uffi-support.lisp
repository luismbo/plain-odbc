;;; -*- Mode: lisp -*-

(in-package :plain-odbc)

(defun %get-string (ptr length)
  (uffi:convert-from-foreign-string ptr :length length :null-terminated-p nil))

(defun %null-ptr () nil)

(uffi:def-array-pointer byte-array-ptr :unsigned-byte) 

(uffi:def-array-pointer uchar-array-ptr :unsigned-char)

(defun %get-binary (ptr length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
    (uffi:with-cast-pointer (aptr ptr 'byte-array-ptr)
      (dotimes (i length)
        (setf (aref res i) (uffi:deref-array aptr :byte i))))
    res))



(defun %put-binary (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (uffi:with-cast-pointer (aptr ptr 'byte-array-ptr)
    (dotimes (i (length vector))
      (setf (uffi:deref-array aptr :byte i) (aref vector i)))))

  
(defun wchar-bytes-to-string (byte-vector)
  (let ((res (make-string (truncate (length byte-vector) 2) :initial-element (code-char 1000))))
    (dotimes (i (truncate (length byte-vector) 2) res)
      (setf (char res i)
              (code-char (+ (aref byte-vector (* 2 i)) (* (aref byte-vector (+ (* 2 i) 1)) 256)))))))


(defun string-to-wchar-bytes (string)
  (EXT:CONVERT-STRING-TO-BYTES string charset:UNICODE-16-LITTLE-ENDIAN))



(defun %put-unicode-string (ptr string)
  (%put-binary ptr (string-to-wchar-bytes string)))


(defun %get-unicode-string (ptr len)
  (wchar-bytes-to-string (%get-binary ptr len)))



(defun %put-str (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (uffi:with-cast-pointer (aptr ptr 'uchar-array-ptr)
    (dotimes (i (length vector))
      (setf (uffi:deref-array aptr :byte i) (aref vector i)))))
