;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common-Lisp; -*-

(defpackage :wiz-util
  (:use :common-lisp :alexandria)
  (:nicknames :wiz)
  (:export :nlet :mlet :while
	   :sfor :n-times :saccumulate :ssum :saverage :sprod :summation :product
	   :existp :split :split-equally
	   :exclusive-or :pow :tak :debug-print :make-number-list
	   :min-position :max-position :general-max/min :grouping :remove-nth
	   :position-if-list :format-list :direct-product
	   :n-times-collect :square :sgn :multiple?
	   :index-list->scalar-index :scalar-index->index-list
	   :binary-list->scalar :radix-num-list->scalar
	   :random-from-probability-list :d :doc :with-open-multiple-file :nthcar :assoc-ref
	   ;; ring-buffer functions
	   :make-ring-buffer :ring-buffer-size :ring-buffer-buffer
	   :ring-buffer-sum :ring-buffer-average
	   :ring-buffer-tail :rb-latest-elem :rb-push!
	   :rb-standard-deviation :rb-standard-deviation-fast
	   :rb-get-sequence :rb-get-difference-sequence
	   :rb-top-elem :rb-tail-elem :rb-min/max
	   ;; list smoother
	   :smoothing-list :smoothing-list-roughly
	   :random-uniform :random-normal :seq :andf :orf
	   ;; for output to gnuplot
	   :plot-list :plot-lists :plot-histogram :plot-histogram-with-pdf
	   :normalize-list
	   :multiplot
	   :with-plot-stream
	   :splot-list
	   ;; for queue
	   :make-q :q-elements :q-key :q-last :q-p
	   :make-empty-queue :enqueue :dequeue :empty-queue?

	   ;; for memoize
	   :memoize-function
	   :unmemoize-function :unmemoize-functions
	   :clear-memoized-function :clear-memoized-functions
	   :function-memoized-p
	   :def-memoized-function
	   :memoized-labels
	   
	   ;; OOP utilities
	   :defclass$

	   ;;; for numerical ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   :matrixp :num-rows :num-cols :square-matrix? :make-matrix
	   :make-identity-matrix :copy-matrix :print-matrix
	   :transpose-matrix :multiply-matrix :add-matrix :subtract-matrix
	   :invert-matrix :solve-matrix :trace-matrix :partial-matrix
	   ;; arrayed-vector and matrix wrapper function
	   :make-vector :list->vector :simple-vector->arrayed-vector
	   :vector-cat :vecot-cat2 :vector-length
	   :euclidean-norm :m* :m+ :ssum-vec :m- :m-t :umat :zero-mat :m-1
	   :m-append-horizon :vec :mat :mapmat :mapvec :cholesky :det-cholesky
	   ;; gaussian
	   :gaussian :multivariate-gaussian :universal-gaussian :random-uniform :random-normal
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	   ;; onlisp functions
	   :last1 :single :append1 :conc1 :mklist :longer :filter :group
	   ; :flatten ; conflict with ALEXANDRIA
	   :prune :find2 :before :after :duplicate :split-if :most :best :mostn
	   :map0-n :map1-n :mapa-b :map->
	   ; :mappend ; conflict with ALEXANDRIA
	   :mvdo-gen
	   ; :shuffle ; conflict with ALEXANDRIA
	   :mapcars
	   :rmapcar
	   :readlist
	   :prompt
	   :break-loop
	   :mkstr
	   :symb
	   :reread
	   :explode
	   :!
	   :def!
	   :memoize
	   ; :compose ; conflict with ALEXANDRIA
	   :fif
	   :fint
	   :fun
	   :lrec
	   :rfind-if
	   :ttrav
	   :trec
	   :condlet-clause
	   :condlet-binds
	   :>casex
	   :dt-args
	   :mvdo-rebind-gen
	   ;; onlisp macros
	   :mac
	   :when-bind
	   :when-bind*
	   ; :with-gensyms ; conflict with ALEXANDRIA
	   :condlet
	   :if3
	   :nif
	   :in
	   :inq
	   :in-if
	   :>case
	   :while
	   :till
	   ;; :for
	   :do-tuples/o
	   :do-tuples/c
	   :mvdo*
	   :mvpsetq
	   :mvdo
	   :allf
	   :nilf
	   :tf
	   :toggle

	   :diff-list

	   ;; アナフォラもexportしないと不便
	   :aif
	   :it
	   :alambda
	   :self
	   ))
