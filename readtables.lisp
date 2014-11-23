;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :wiz-util)

(defun read-debug-print (stream char)
  (declare (ignore char))
  (let ((read-data (read stream t nil t)))
    `(debug-print (quote ,read-data) ,read-data)))

(defreadtable :default-readtable
  (:merge :current))

(defreadtable :wiz-readtable
  (:merge :standard)
  (:macro-char #\@ #'read-debug-print))
