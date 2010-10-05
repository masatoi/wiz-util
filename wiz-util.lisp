;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common-Lisp; -*-

(defpackage :wiz-util
  (:use :common-lisp)
  (:nicknames :wiz)
  (:export :mac :nlet :mlet :while
	   :sfor :n-times :saccumulate :ssum :saverage :sprod :summation :product
	   :last1 :single? :mklist :append1 :nconc1 :existp :split
	   :exclusive-or :pow :tak :debug-print :make-number-list
	   :min-position :max-position :general-max/min :filter :grouping :remove-nth
	   :position-if-list :format-list :direct-product
	   :n-times-collect :square :sgn :index-list->scalar-index :scalar-index->index-list
	   :random-from-probability-list :d :with-open-multiple-file :flatten :nthcar :assoc-ref))

(in-package :wiz-util)

;;; マクロを一階層展開して表示するマクロ.
(defmacro mac (expression)
  `(pprint (macroexpand-1 (quote ,expression))))

;;; nlet ; named-let ; 名前付きlet
;;  Schemeのnamed-letと同じ、繰り返し構造の簡易表現
(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (declare (optimize (speed 3))) ; for tail recursion optimization
     (,tag ,@(mapcar #'cadr var-vals))))

;; ;; from LOL
;; (defun mkstr (&rest args)
;;   (with-output-to-string (s)
;;     (dolist (a args) (princ a s))))

;; (defun symb (&rest args)
;;   (values (intern (apply #'mkstr args))))

;; (defun g!-symbol-p (s)
;;   (and (symbolp s)
;;        (> (length (symbol-name s)) 2)
;;        (string= (symbol-name s)
;;                 "G!"
;;                 :start1 0
;;                 :end1 2)))

;; (defmacro defmacro/g! (name args &rest body)
;;   (let ((syms (remove-duplicates
;;                 (remove-if-not #'g!-symbol-p
;;                                (flatten body)))))
;;     `(defmacro ,name ,args
;;        (let ,(mapcar
;;                (lambda (s)
;;                  `(,s (gensym ,(subseq
;;                                  (symbol-name s)
;;                                  2))))
;;                syms)
;;          ,@body))))

;; (defun o!-symbol-p (s)
;;   (and (symbolp s)
;;        (> (length (symbol-name s)) 2)
;;        (string= (symbol-name s)
;;                 "O!"
;;                 :start1 0
;;                 :end1 2)))

;; (defun o!-symbol-to-g!-symbol (s)
;;   (symb "G!"
;;         (subseq (symbol-name s) 2)))

;; (defmacro defmacro! (name args &rest body)
;;   (let* ((os (remove-if-not #'o!-symbol-p args))
;;          (gs (mapcar #'o!-symbol-to-g!-symbol os)))
;;     `(defmacro/g! ,name ,args
;;        `(let ,(mapcar #'list (list ,@gs) (list ,@os))
;;           ,(progn ,@body)))))

;; ;; 従来型のnletは実は末尾再帰呼出しじゃなくても使えるが、これは厳密に末尾再帰呼出しである必要がある
;; (defmacro! nlet-tail (n letargs &rest body)
;;   (let ((gs (loop for i in letargs
;; 		 collect (gensym))))
;;     `(macrolet
;; 	 ((,n ,gs
;; 	    `(progn
;; 	       (psetq
;; 		,@(apply #'nconc
;; 			 (mapcar
;; 			  #'list
;; 			  ',(mapcar #'car letargs)
;; 			  (list ,@gs))))
;; 	       (go ,',g!n))))
;;        (block ,g!b
;; 	 (let ,letargs
;; 	   (tagbody
;; 	      ,g!n (return-from
;; 		    ,g!b (progn ,@body))))))))


;;; mlet ; 多重let
;; 値のリストの要素をシンボルのリストの要素に束縛するletのシンタックスシュガー
(defmacro mlet (symbols value-list &body body)
  `(apply (lambda ,symbols ,@body) ,value-list))

;;; while 驚くべきことに標準では入っていない.
(defmacro while (test &rest body)
  `(do ()
    ((not ,test))
    ,@body))

;; sfor ; simple for
;;  (sfor (i 0 10)
;;     (princ i)) のように使う.
;; OnLispのforと違うところは,最後に評価されたbodyの値が返るということ.
(defmacro sfor ((index-var start-position stop-position) &body body)
  `(let ((,index-var ,start-position))
     (labels ((rec ()
		(if (= ,index-var ,stop-position)
		    (progn ,@body)
		    (progn ,@body
			   (setf ,index-var (1+ ,index-var))
			   (rec)))))
       (rec))))

;;; 典型的n-times
(defmacro n-times (m &body body)
  `(labels ((rec (n cnt)
	     (cond ((= n 0) nil)
		   ((= cnt n) (progn ,@body))
		   (t (progn ,@body
			     (rec n (1+ cnt)))))))
    (rec ,m 1)))

;;; saccumulate (SICP)
(defun saccumulate (op initial sequence)
  (nlet itr ((seq sequence)
	     (prod initial))
    (if (null seq)
	prod
	(itr (cdr seq) (funcall op prod (car seq))))))

;;; simple sum
(defun ssum (lst)
  (saccumulate #'+ 0 lst))

;;; simple average
(defun saverage (lst)
  (/ (ssum lst) (length lst)))

;;; simple sum
(defun sprod (lst)
  (saccumulate #'* 1 lst))

;;; 総和を取るマクロ
(defmacro summation ((index start end) &body body)
  `(nlet iter ((product 0)
	       (,index ,start))
    (if (> ,index ,end)
	product
	(iter (+ (progn ,@body) product) (1+ ,index)))))

;;; 総積を取るマクロ
(defmacro product ((index start end) &body body)
  `(nlet iter ((product 1)
	       (,index ,start))
    (if (> ,index ,end)
	product
	(iter (* (progn ,@body) product) (1+ ,index)))))
		 
;;; On Lisp Chapter 4 Utirity Functions

(proclaim '(inline last1 single append1 conc1 mklist))

;;; List Operator
;; Return last element of list.
(defun last1 (lst)
  (car (last lst)))

;; Return true if the list consist of only one element.
(defun single? (lst)
  (and (consp lst)
       (not (cdr lst))))

;; make list; nevertheless argument is atom.
(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

;; append atom to list.
(defun append1 (lst obj)
  (append lst (list obj)))

;; append atom to list (with side effect).
(defun nconc1 (lst obj)
  (nconc lst (list obj)))

;; lstにpredを満たすような要素が存在するならその要素を返す.
(defun existp (pred lst)
  (if (null lst)
      nil
      (if (funcall pred (car lst))
	  (car lst)
	  (existp pred (cdr lst)))))

;; リストを先頭n個で分割し,前半n個と残りのリストのリストを返す
(defun split (lst n)
  (nlet itf ((i 1)
	     (product '())
	     (remainder lst))
    (if (or (> i n)
	    (null remainder))
	(list (nreverse product) remainder)
	(itf (1+ i)
	     (cons (car remainder) product)
	     (cdr remainder)))))

;;; 排他的論理和
(defmacro exclusive-or (a b)
  `(and (or ,a ,b)
       (not (and ,a ,b))))

;;; べき乗 ;exptは複素数にも対応する
(defun pow (n a)
  (labels ((pow-iter (n a product)
	     (if (zerop a)
		 product
		 (pow-iter n (1- a) (* n product)))))
    (pow-iter n a 1)))

;;; 竹内関数
(defun tak (x y z)
  (if (<= x y)
      y
      (tak (tak (1- x) y z)
	   (tak (1- y) z x)
	   (tak (1- z) x y))))

;;; デバッグ用reader macro
;;; 式の前に「@」をつけるとdebug-printが適用される.
;;; 例 @(+ 1 2)
(defun debug-print (pre-exp exp)
    (format t "~A => ~A~%" pre-exp exp)
    exp)

(set-macro-character
   #\@
   #'(lambda (stream char)
       char ;for avoid Style-Warning
       (let ((read-data (read stream t nil t)))
	 `(debug-print (quote ,read-data) ,read-data))))

;; (set-dispatch-macro-character #\# #\=
;;    #'(lambda (stream char1 char2)
;;        (declare (ignore char1 char2 char3)) ;for avoid Style-Warning
;;        (let ((read-data (read stream t nil t)))
;; 	 `(debug-print (quote ,read-data) ,read-data))))

;;; デリミタリードマクロ
;;; 例. #[2 7] => (2 3 4 5 6 7)
(defun make-number-list (start end)
  (nlet iter ((i start)
	      (product nil))
	(if (> i end)
	    product
	    (iter (1+ i) (append1 product i)))))

(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
  #'(lambda (stream char1 char2)
      char1 ;for avoid Style-Warning
      char2
      (let ((pair (cons 'list (read-delimited-list #\] stream t)))) ;先の例だと(2 7)
	`(apply #'make-number-list ,pair))))

;; リストの中で最大/最小の値を持つ要素の位置を返す関数
(defun min-position (lst)
  (let ((min-value (apply #'min lst)))
    (position-if (lambda (x)
		   (= x min-value)) lst)))
(defun max-position (lst)
  (let ((max-value (apply #'max lst)))
    (position-if (lambda (x)
		   (= x max-value)) lst)))

;;; max/minの一般化
;; predicateは2引数を取る関数
;; example
;; (general-max/min #'> '(3 2 4 1)) => 4
;; (general-max/min #'string>  '("age" "hoge" "fuga")) => "hoge"
(defun general-max/min (predicate lst)
  (nlet itr ((max/min-elem (car lst))
	     (lst (cdr lst)))
    (if (null lst)
	max/min-elem
	(itr (if (funcall predicate max/min-elem (car lst))
		 max/min-elem
		 (car lst))
	     (cdr lst)))))

;;; remove-if-notと同じ
(defun filter (proc lst)
  (nlet iter ((product '())
	      (lst lst))
    (if (null lst)
	(nreverse product)
	(if (funcall proc (car lst))
	    (iter (cons (car lst) product)
		  (cdr lst))
	    (iter product (cdr lst))))))

;;; 複数の述語でリストをグルーピングする
;; (grouping (list #'numberp #'symbolp) '(a 1 b 2 c 3))
;; => ((1 2 3) (A B C))

(defun grouping (predicate-list target-list)
  (mapcar (lambda (predicate)
	    (remove-if-not predicate target-list))
	  predicate-list))

;; lstからn番目の値を抜いたリストと抜いた値を多値にして返す
(defun remove-nth (n lst)
  (nlet itr ((i 0)
	     (lst lst)
	     (product '()))
    (if (= i n)
	(values (append (nreverse product) (cdr lst)) (car lst))
	(itr (1+ i) (cdr lst) (cons (car lst) product)))))

;; ;;; リストによるキューの実装(ANSI Common Lisp P178)
;; ;;; ちゃんとCLOSで実装されたqueueはqueue.lispに

;; (defun make-queue () (cons nil nil))
;; (defun enqueue (obj q)
;;   (if (null (car q))
;;       (setf (cdr q) (setf (car q) (list obj)))
;;       (setf (cdr (cdr q)) (list obj)
;; 	    (cdr q) (cdr (cdr q))))
;;   (car q))
;; (defun dequeue (q)
;;   (pop (car q)))

;; (defun queue-length (q)
;;   (length (car q)))

;;; for each element of sequence, if pridicate is true then return list of positions.
;; position-ifはsequenceに条件を満たす複数の要素があるときに位置のリストを返せない
(defun position-if-list (pridicate sequence)
  (nlet itr ((i 0)
	     (sequence sequence)
	     (product ()))
    (if (null sequence)
	(nreverse product)
	(if (funcall pridicate (car sequence))
	    (itr (1+ i) (cdr sequence) (cons i product))
	    (itr (1+ i) (cdr sequence) product)))))

(defun format-list (lst &key (stream t))
  (nlet itr ((lst lst))
    (if (null lst)
	(format stream "~%")
	(let ((elem (car lst)))
	  (cond ((integerp elem) (format stream "~A " elem))
		((floatp elem) (format stream "~6$ " elem))
		(t (format stream "~A " elem)))		 
	  (itr (cdr lst))))))

;;; リストの直積をとる
;;; 例) (direct-product lst1 lst2 lst3)
(defun direct-product-2arg (lst1 lst2)
  (let ((product ()))
    (dolist (l1 lst1)
      (dolist (l2 lst2)
	(setf product (cons
		       (if (listp l2)
			   (cons l1 l2)
			   (list l1 l2)) product))))
    (reverse product)))

(defun direct-product (&rest lsts)
  (let ((lsts (reverse lsts)))
    (nlet iter ((lsts (cdr lsts))
		(product (car lsts)))
      (if (null lsts)
	  product
	  (iter (cdr lsts) (direct-product-2arg (car lsts) product))))))

;;; n-times-collect
;;; bodyをN回実行した結果をリストして返す 
;;  cl-utilitiesのcollectingでも同様のことができる
(defmacro n-times-collect (n &body body)
  `(labels ((rec (m cnt product)
	      (cond ((= m 0) nil)
		    ((= cnt m) (cons (progn ,@body) product))
		    (t (rec m (1+ cnt)
			    (cons (progn ,@body) product))))))
     (rec ,n 1 ())))

(defun square (x)
  (* x x))

(defun sgn (x)
  (if (>= x 0) 1 -1))

;;; 多次元配列から整数のインデックスへ変換

;; ;; 例
;; (sfor (i 0 2)
;;   (sfor (j 0 3)
;;     (sfor (k 0 1)
;;       (let* ((sindex (index-list->scalar-index '(3 4 2) (list i j k)))
;; 	     (index-lst (scalar-index->index-list '(3 4 2) sindex)))
;; 	(format t "(~A ~A ~A) => ~A => ~A~%" i j k sindex index-lst)))))

(defun index-list->scalar-index (dimension-list index-list)
  (nlet itr ((dim-lst (cdr dimension-list))
	     (i-lst index-list)
	     (product 0))
    (if (null dim-lst)
	(+ product (car i-lst))
	(itr (cdr dim-lst) (cdr i-lst)
	     (+ (apply #'* (car i-lst) dim-lst)
		product)))))

;;; 整数のインデックスから多次元配列のインデックスリストへ

(defun scalar-index->index-list (dimension-list scalar-index)
  (nlet itr ((dim-lst (reverse dimension-list))
	     (i-lst '())
	     (num scalar-index))
    (if (= (length dim-lst) 1)
	(cons num i-lst)
	(let ((remainder (mod num (car dim-lst))))
	  (itr (cdr dim-lst)
	       (cons remainder i-lst)
	       (/ (- num remainder) (car dim-lst)))))))

;;; 総和が1になるような確率のリストからその確率分布に従ってサンプルし,位置を返す
(defun random-from-probability-list (probability-list)
  (let ((prob-sum-list
	 (nlet iter ((p-lst probability-list)
		     (sum 0)
		     (product '()))
	   (if (= (length p-lst) 1)
	       (reverse (cons 1.0 product))
	       (let ((prob-sum (+ sum (car p-lst))))
		 (iter (cdr p-lst) prob-sum (cons prob-sum product))))))
	(rand (random 1.0d0)))
    (position-if (lambda (x)
		   (< rand x)) prob-sum-list)))

;;; describeのラッパー
(defun d (object)
  (describe object))

;;; 複数のファイルストリームを開いて何かするマクロ
;; ; before
;; (with-open-multiple-file ((f1 "/hoge/fuga" :direction :input)
;; 			  (f2 "/hoge/mage" :direction :output))
;;   (print (read f1) f2))

;; ; after
;; (WITH-OPEN-FILE (F2 "/hoge/mage" :DIRECTION :OUTPUT)
;;   (WITH-OPEN-FILE (F1 "/hoge/fuga" :DIRECTION :INPUT)
;;     (PROGN (PRINT (READ F1) F2))))

(defmacro with-open-multiple-file (stream-specs &body body)
  (nlet itr ((stream-specs stream-specs)
	     (product '())
	     (body body))
    (if (null stream-specs)
	product
	(itr (cdr stream-specs)
	     `(with-open-file (,@(car stream-specs))
		,(if body (cons 'progn body) product))
	     nil))))

;;; リストの入れ子構造をなくす(木構造の葉のみからなるリストを返す)
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

;;; 標準ではnthcdrがあってnthcarがないので
(defun nthcar (n list)
  (nlet itr ((n n)
	     (list list)
	     (product '()))
    (if (or (zerop n) (null list))
	(nreverse product)
	(itr (1- n) (cdr list) (cons (car list) product)))))

(defun assoc-ref (item alist)
  (cadr (assoc item alist)))