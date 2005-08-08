;;; -*- Mode: lisp -*-

(in-package :plain-odbc)


(defun %get-string (ptr length)
  (uffi:convert-from-foreign-string ptr :length length :null-terminated-p nil))

(defun %null-ptr () 
  (uffi:make-null-pointer :void))

(uffi:def-array-pointer ubyte-array-ptr :unsigned-byte) 

(uffi:def-array-pointer uchar-array-ptr :unsigned-byte)

#-clisp
(defun %get-binary (ptr length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
    (uffi:with-cast-pointer (aptr ptr :unsigned-byte)
      (dotimes (i length)
        (setf (aref res i) (uffi:deref-array aptr '(:array :unsigned-byte) i))))
    res))

#-clisp
(defun %put-binary (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (uffi:with-cast-pointer (aptr ptr :unsigned-byte)
    (dotimes (i (length vector))
      (setf (uffi:deref-array aptr '(:array :unsigned-byte) i) (aref vector i)))))


#+clisp
(defun %get-binary (ptr length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
    (uffi:with-cast-pointer (aptr ptr 'ubyte-array-ptr)
      (dotimes (i length)
        (setf (aref res i) (uffi:deref-array aptr '(:array :unsigned-byte) i))))
    res))

#+clisp
(defun %put-binary (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (uffi:with-cast-pointer (aptr ptr 'ubyte-array-ptr)
    (dotimes (i (length vector))
      (setf (uffi:deref-array aptr '(:array :unsigned-byte) i) (aref vector i)))))



  
(defun wchar-bytes-to-string (byte-vector)
  (let ((res (make-string (truncate (length byte-vector) 2) :initial-element (code-char 1000))))
    (dotimes (i (truncate (length byte-vector) 2) res)
      (setf (char res i)
              (code-char (+ (aref byte-vector (* 2 i)) (* (aref byte-vector (+ (* 2 i) 1)) 256)))))))

#+clisp
(defun string-to-wchar-bytes (string)
  (EXT:CONVERT-STRING-TO-BYTES string charset:UNICODE-16-LITTLE-ENDIAN))

#-clisp
(defun string-to-wchar-bytes (string)
  (let ((vec (make-array (* 2 (length string)) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string))
      (let ((k (char-code (char string i))))
        (setf (aref vec (* 2 i)) (logand k 255)
              (aref vec (1+ (* 2 i))) (ash k -8))))
    vec))

(defun %put-unicode-string (ptr string)
  (%put-binary ptr (string-to-wchar-bytes string)))


(defun %get-unicode-string (ptr len)
  (wchar-bytes-to-string (%get-binary ptr len)))


#-clisp
(defun %put-str (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (uffi:with-cast-pointer (aptr ptr :unsigned-char)
    (dotimes (i (length vector))
      (setf (uffi:deref-array aptr 'uchar-array-ptr i) (char-code (aref vector i))))))

#+clisp
(defun %put-str (ptr vector &optional max-length)
  (when (and max-length (> (length vector) max-length))
    (error "vector of length ~d is longer than max-length: ~d"
           (length vector) max-length))
  (uffi:with-cast-pointer (aptr ptr 'uchar-array-ptr)
    (dotimes (i (length vector))
      (setf (uffi:deref-array aptr 'uchar-array-ptr i) (char-code (aref vector i))))))


(defun make-unicode-string (length)
  #+clisp
  (make-string length)
  #+allegro
  (make-string length )
  #+lispworks
  (make-string length :initial-element (code-char 300)))
