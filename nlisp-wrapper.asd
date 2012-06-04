;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common-Lisp; -*-

(defsystem "nlisp-wrapper"
    :description "wrapper for nlisp"
    :version "0.01"
    :author "Satoshi Imai <satoshi.imai@gmail.com>"
    :licence "Public Domain"
    :depends-on (:wiz-util :nlisp)
    :components ((:file "nlisp-wrapper")))
