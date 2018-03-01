;;; -*- Coding: utf-8; Mode: Lisp; -*-

(defsystem "wiz-util"
    :description "wiz-util: utility collections for my daily work."
    :version "0.8"
    :author "Satoshi Imai <satoshi.imai@gmail.com>"
    :licence "Public Domain"
    :depends-on (:alexandria
		 :anaphora
		 :metatilities
		 :moptilities
		 :metabang-bind
                 :uiop
                 :cl-ppcre
                 :iterate
		 )

    :components ((:file "packages")
                 (:file "typed" :depends-on ("packages"))
		 (:file "onlisp" :depends-on ("packages"))
		 (:file "wiz-util" :depends-on ("packages"))
		 (:file "memoize" :depends-on ("packages"))
		 (:file "queue" :depends-on ("packages"))
                 (:file "scripting" :depends-on ("packages" "wiz-util"))
		 (:file "matrix" :depends-on ("packages" "wiz-util"))
		 (:file "multivariate-gaussian" :depends-on ("packages" "matrix"))))
