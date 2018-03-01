;;; -*- Coding: utf-8; Mode: Lisp; -*-

(in-package :wiz-util)

(defparameter *optimize-settings*
  '(optimize (speed 3) (safety 0) (debug 0)))

(defmacro defn (function-spec (&rest arg-specs) &body body)
  (assert (listp function-spec))
  (assert (listp arg-specs))
  (dolist (arg-spec arg-specs)
    (assert (listp arg-spec))
    (assert (= (length arg-spec) 2)))
  `(progn
     (declaim (ftype (function ,(mapcar #'cadr arg-specs) ,(cadr function-spec)) ,(car function-spec)))
     (defun ,(car function-spec) ,(mapcar #'car arg-specs)
       (declare ,*optimize-settings*
                ,@(mapcar (lambda (arg arg-type)
                            (list 'type arg-type arg))
                          (mapcar #'car arg-specs)
                          (mapcar #'cadr arg-specs)))
       ,@body)))

(defmacro tlet (bindings &body body)
  (assert (listp bindings))
  (dolist (binding bindings)
    (assert (listp binding))
    (assert (= (length binding) 3)))
  `(let (,@(mapcar (lambda (binding)
                     (subseq binding 0 2))
                   bindings))
     (declare ,@(mapcar (lambda (binding)
                          (list 'type (caddr binding) (car binding)))
                        bindings))
     ,@body))
