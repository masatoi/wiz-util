#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#

(ql:quickload :wiz-util :silent t)
(in-package :wiz-util)

(defun main (&rest argv)
  (declare (ignorable argv))

  (let ((files (ls)))
    (loop for i from 1 to (length files)
          for f in files
          do
       (print (list "mv"
                    (format-pathname f)
                    (format nil "~3,'0d.wav" i))))))
