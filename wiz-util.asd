;;; -*- Coding: utf-8; Mode: Lisp; -*-

(defsystem "wiz-util"
    :description "wiz-util: utility collections for my daily work."
    :version "0.6"
    :author "Satoshi Imai <satoshi.imai@gmail.com>"
    :licence "Public Domain"
    :depends-on (:alexandria
		 :cl-fad
		 ;; :lparallel
		 :anaphora
		 :metatilities
		 :moptilities
		 :metabang-bind
		 ;; :named-readtables
                 :iterate
		 )

    :components ((:file "packages")
		 ;;(:file "readtables")
		 (:file "onlisp" :depends-on ("packages"))
		 (:file "lol-for-nlet" :depends-on ("packages" "onlisp"))
		 (:file "wiz-util" :depends-on ("packages" "lol-for-nlet"))
		 (:file "memoize" :depends-on ("packages"))
		 ;;(:file "lol" :depends-on ("packages"))
		 (:file "queue" :depends-on ("packages"))
		 (:file "matrix" :depends-on ("packages" "wiz-util"))
		 (:file "multivariate-gaussian" :depends-on ("packages" "matrix"))))
