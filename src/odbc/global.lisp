;;; -*- Mode: Lisp -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt


(in-package :plain-odbc)

(defvar *universal-time-to-date-dataype* 'identity)
(defvar *date-datatype-to-universal-time* 'identity)
(defvar *date-type-predicate* (lambda (x) nil))

(defvar *max-precision* 65536)

(defvar *lob-fetch-buffer-size* 100000)

(defvar *max-column-buffer-size* 8001)

(defvar *default-string-parameter-size* 8001)

(defvar  *DEFAULT-BINARY-PARAMETER-SIZE* 8001)

