;;; -*- Coding: utf-8; Mode: Lisp; -*-

(in-package :wiz-util)

;; ;; settings for multiprocessing
;; (defparameter lparallel:*kernel* (lparallel:make-kernel 20))

;;; lambdaの短縮表記 
;;  http://qiita.com/AlgoSystem/items/eb45113b173fd99d5605
;;  組合せ式の最初の位置では使えないので完全なlambdaの置き換えにはリードマクロが必要だが、通常利用ではこれで十分
(defmacro ^ ((&rest parameter) &body body)
  `(lambda ,parameter ,@body))

;;; nlet ; named-let ; 名前付きlet
;;  Schemeのnamed-letと同じ、繰り返し構造の簡易表現
(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (declare (optimize (speed 3))) ; for tail recursion optimization
     (,tag ,@(mapcar #'cadr var-vals))))

;; 従来型のnletは実は末尾再帰呼出しじゃなくても使えるが、これは厳密に末尾再帰呼出しである必要がある
;; (defmacro! nlet (tag letargs &rest body)
;;   (let ((gs (loop for i in letargs
;; 		 collect (gensym))))
;;     `(macrolet
;; 	 ((,tag ,gs
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
;; destructuring-bind とほぼ同じ
(defmacro mlet (symbols value-list &body body)
  "Evaluate the body with binding each value of value-list to the symbols.
Examples: 
 (mlet (a b c) '(1 2 3)
   (list a b c))
 => (1 2 3)"
  `(apply (lambda ,symbols ,@body) ,value-list))

;;; while 驚くべきことに標準では入っていない.
(defmacro while (test &rest body)
  `(do ()
    ((not ,test))
    ,@body))

(defmacro until (test &rest body)
  `(do ()
    (,test)
    ,@body))

;;; dolist-with-counter 
(defmacro dolist-with-counter ((list-elem-var counter-var list) &body body)
  `(let ((,counter-var 0))
     (dolist (,list-elem-var ,list)
       ,@body
       (incf ,counter-var))))

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
;;; reduceとほぼ等価
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

;; リストをn等分する
(defun split-equally (lst n)
  (let ((m (floor (length lst) n)))
    (nlet iter ((lst lst) (product nil))
      (if (< (length lst) m)
	  (nreverse (cons (append (car product) lst) (cdr product)))
	  (iter (nthcdr m lst) (cons (nthcar m lst) product))))))

;; ベクタをシャッフルする
(defun shuffle-vector (vec)
  (loop for i from (1- (length vec)) downto 1 do
    (let* ((j (random (1+ i)))
	   (tmp (svref vec i)))
      (setf (svref vec i) (svref vec j))
      (setf (svref vec j) tmp)))
  vec)

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

;;; describeのラッパー
(defun d (object)
  "wrapper of describe."
  (describe object))

;;; documentationのラッパー
(defun doc (name-symbol)
  "simplified documentation function."
  (aif (documentation name-symbol 'function)
       (format t "~A~%" it))
  (aif (documentation name-symbol 'variable)
       (format t "~A~%" it)))

;;; デバッグ用reader macro
;;; 式の前に「≫」をつけるとdebug-printが適用される.
;;; 例 ≫(+ 1 2)
(defun debug-print (pre-exp exp)
    (format t "~A => ~A~%" pre-exp exp)
    exp)

(set-macro-character
 #\MUCH_GREATER-THAN
 #'(lambda (stream char)
     (declare (ignore char))
     (let ((read-data (read stream t nil t)))
       `(debug-print (quote ,read-data) ,read-data))))

(defun describe-print (symbol)
  (let ((var-description
	 (if (boundp symbol)
	   (with-output-to-string (s)
	     (format s "=== Variable description ==========~%")
	     (describe symbol s))))
	(func-description
	 (if (fboundp symbol)	   
	   (with-output-to-string (s)
	     (format s "=== Function description ==========~%")
	     (describe symbol s)))))
    (if (or var-description func-description)
      (let ((str (concatenate 'string var-description func-description)))
	(format t str)
	str)
      "Not found")))

(set-macro-character
 #\REFERENCE_MARK
 #'(lambda (stream char)
     (declare (ignore char))
     (let ((read-data (read stream t nil t)))
       `(describe-print (quote ,read-data)))))

;;; @は他のライブラリでマクロ文字として多用されるため、Gaucheのデバックプリント風の2ストローク表記に
;; (make-dispatch-macro-character #\REFERENCE_MARK)

;; (set-dispatch-macro-character #\REFERENCE_MARK #\=
;;    #'(lambda (stream char1 char2)
;;        (declare (ignore char1 char2))
;;        (let ((read-data (read stream t nil t)))
;; 	 `(debug-print (quote ,read-data) ,read-data))))

;;; デリミタリードマクロ
;;; 例. #[2 7] => (2 3 4 5 6 7)
;; (defun make-number-list (start end)
;;   (nlet iter ((i start)
;; 	      (product nil))
;; 	(if (> i end)
;; 	    product
;; 	    (iter (1+ i) (append1 product i)))))

;; (set-macro-character #\] (get-macro-character #\)))
;; (set-dispatch-macro-character #\# #\[
;;   #'(lambda (stream char1 char2)
;;       char1 ;for avoid Style-Warning
;;       char2
;;       (let ((pair (cons 'list (read-delimited-list #\] stream t)))) ;先の例だと(2 7)
;; 	`(apply #'make-number-list ,pair))))

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

(defmacro square (x)
  (let ((val (gensym)))
    `(let ((,val ,x))
       (* ,val ,val))))

;; (defun square (x)
;;   (* x x))

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

;;; 基数が固定の場合
(defun radix-num-list->scalar (radix radix-num-list)
  (index-list->scalar-index (make-list (length radix-num-list) :initial-element radix) radix-num-list))

;;; 2進リストの場合
(defun binary-list->scalar (binary-list)
  (radix-num-list->scalar 2 binary-list))

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

;;; for bit-vector

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

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

(defun openi (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file :direction :input
        :element-type element-type
        :external-format external-format))
(defun openo (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :supersede
        :if-does-not-exist :create))

(defun opena (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :append
        :if-does-not-exist :create))

(defmacro with-stream ((var stream) &body forms-decls)
  `(with-open-stream (,var ,stream) ,@forms-decls))

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

;;; リングバッファ
(defstruct ring-buffer
  (size 0)
  (buffer nil)
  (average 0d0)
  (tail 0))

;; リングバッファにデータをプッシュし、最も古いデータがポップされ、返される(バッファのサイズは一定)
;; 2乗の平均から平均の2乗を引くことで分散が求められる
(defun rb-push! (ring-buffer datum)
  (let ((size (ring-buffer-size ring-buffer)))
    (cond ((zerop size)
           (error "ring-buffer size must be set initially."))
          ;; bufferが空のとき
          ((null (ring-buffer-buffer ring-buffer))
           (setf (ring-buffer-buffer ring-buffer)
                 (make-array size :initial-element datum))
           (setf (ring-buffer-average ring-buffer) datum)
           (setf (ring-buffer-tail ring-buffer) (1- size))
           datum)
          ;; bufferに何か入っているとき
          (t
           (let* ((pop-datum-position (mod (1+ (ring-buffer-tail ring-buffer)) size))
                  (pop-datum (svref (ring-buffer-buffer ring-buffer) pop-datum-position)))
             (setf (svref (ring-buffer-buffer ring-buffer) pop-datum-position) datum) ; push
             (setf (ring-buffer-average ring-buffer)
                   (+ (ring-buffer-average ring-buffer)
                      (/ (- datum pop-datum) size)))
             (setf (ring-buffer-tail ring-buffer) pop-datum-position)
             pop-datum)))))

(defun rb-latest-elem (ring-buffer)
  (let ((buff (ring-buffer-buffer ring-buffer))
	(tail (ring-buffer-tail ring-buffer)))
    (if buff (aref buff tail))))

;; 不偏分散
(defun rb-variance (ring-buffer)
  (let ((ave (ring-buffer-average ring-buffer))
	(buff (ring-buffer-buffer ring-buffer))
	(size (ring-buffer-size ring-buffer)))
    (/ (loop for i from 0 to (1- size)
		summing (square (- ave (aref buff i))))
	     (1- size))))

;; 不偏分散の平方根
(defun rb-standard-deviation (ring-buffer)
  (let ((ave (ring-buffer-average ring-buffer))
	(buff (ring-buffer-buffer ring-buffer))
	(size (ring-buffer-size ring-buffer)))
    (sqrt (/ (loop for i from 0 to (1- size)
		summing (square (- ave (aref buff i))))
	     (1- size)))))
       
;; リングバッファに現在入っているデータのベクタを取り出す
(defun rb-get-sequence (ring-buffer)
  (let ((size (ring-buffer-size ring-buffer))
	(tail (ring-buffer-tail ring-buffer))
	(buff (ring-buffer-buffer ring-buffer))
	(product (make-array (ring-buffer-size ring-buffer))))
    (loop for i from 0 to (1- size)
	 do (if (<= i tail)
		(let ((queue-position (+ (- (1- size) tail) i)))
		  (setf (aref product queue-position) (aref buff i)))
		(let ((queue-position (1- (- i tail))))
		  (setf (aref product queue-position) (aref buff i)))))
    product))

;; リングバッファに入っているレートから差分系列ベクタを取り出す
;; 差分系列なのでリングバッファのサイズよりは1つ短くなる
(defun rb-get-difference-sequence (ring-buffer)
  (let* ((size (ring-buffer-size ring-buffer))
	 (seq  (rb-get-sequence ring-buffer))
	 (diff (make-array (1- size))))
    (loop for i from 1 to (1- size)
	 do (setf (aref diff (1- i)) (- (aref seq i)  (aref seq (1- i)))))
    diff))

(defun rb-top-elem (rb)
  (svref (ring-buffer-buffer rb) (mod (1+ (ring-buffer-tail rb)) (ring-buffer-size rb))))

(defun rb-tail-elem (rb)
  (svref (ring-buffer-buffer rb) (ring-buffer-tail rb)))

(defun rb-min/max (rb)
  (loop for i from 0 to (1- (ring-buffer-size rb))
	maximizing (svref (ring-buffer-buffer rb) i) into max
	minimizing (svref (ring-buffer-buffer rb) i) into min
	finally (return (values min max))))

;;; リストを移動平均で平滑化する
(defun smoothing-list (list sma)
  (nlet iter ((rb (make-ring-buffer :size sma))
	      (list list)
	      (product nil))
    (if (null list)
	(nreverse product)
	(progn
	  (rb-push! rb (car list))
	  (iter rb (cdr list) (cons (ring-buffer-average rb) product))))))

(defun smoothing-list-roughly (list sma)
  (nlet iter ((rb (make-ring-buffer :size sma))
	      (list list)
	      (product nil)
	      (cnt sma))
    (if (null list)
	(nreverse product)
	(progn
	  (rb-push! rb (car list))
	  (if (zerop cnt)
	      (iter rb (cdr list) (cons (ring-buffer-average rb) product) sma)
	      (iter rb (cdr list) product (1- cnt)))))))

;; 一様分布からのサンプリング
(defun random-uniform (start end)
  (+ (random (- end start)) start))

;; Box-Muller法による1次元正規分布のサンプリング
(defun random-normal (&key (mean 0d0) (sd 1d0))
  (let ((alpha (random 1.0d0))
	(beta  (random 1.0d0)))
    (+ (* sd
	  (sqrt (* -2 (log alpha)))
	  (sin (* 2 pi beta)))
       mean)))

(defun seq (start end &key (by 1) (n nil))
  (if n
      (loop for i from start to end by (/ (- end start) n) collect i)
      (nlet itf ((i end)
		 (product '()))
	(if (< i start)
	    product
	    (itf (- i by) (cons i product))))))

;;; OOP utilities

;; defclass-simplified: Simplified definition of classes which similar to definition of structure.
(defmacro defclass$ (class-name superclass-list &body body)
  "Simplified definition of classes which similar to definition of structure.
 [Example]
  (defclass$ agent (superclass1 superclass2)
    currency
    position-list
    (position-upper-bound 1)
    log
    money-management-rule)
=> #<STANDARD-CLASS AGENT>"
  `(defclass ,class-name (,@superclass-list)
     ,(mapcar (lambda (slot)
		(let* ((slot-symbol (if (listp slot) (car slot) slot))
		       (slot-name (symbol-name slot-symbol))
		       (slot-initval (if (listp slot) (cadr slot) nil)))
		  (list slot-symbol
			:accessor (intern (concatenate 'string slot-name "-OF"))
			:initarg (intern slot-name :keyword)
			:initform slot-initval)))
	      body)))

;;; 真理値のリストを取って、論理積/論理和をとる関数
;; and/orはマクロなので(apply #'and '(t nil t))のようなことができない。
(defun andf (truth-val-list)
  (cond ((null truth-val-list) t)
	((car truth-val-list) (andf (cdr truth-val-list)))
	(t nil)))

(defun orf (truth-val-list)
  (cond ((null truth-val-list) nil)
	((car truth-val-list) t)
	(t (orf (cdr truth-val-list)))))

;;; Clojure風の無名関数のためのシンタックスシュガー
;;; 使用例: #L(* $1 $1) で1引数を取って二乗する無名関数に展開される
;;;         #2L(* $1 $2) で2引数を取って積を返す無名関数に展開される
(defun sharpL-reader (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg collect (symb '$ i))
     ,(read stream t nil t)))

(set-dispatch-macro-character #\# #\L #'sharpL-reader)

;;; リストから階差数列を作って返す関数
(defun diff-list (lst)
  (nlet iter ((lst lst)
	      (product nil))
    (let ((cdr-lst (cdr lst)))
      (if (null cdr-lst)
	  (nreverse product)
	  (iter cdr-lst (cons (- (car cdr-lst) (car lst)) product))))))

;;; リストから対数差分系列を作って返す関数
(defun log-diff-list (lst)
  (nlet iter ((lst lst)
	      (product nil))
    (let ((cdr-lst (cdr lst)))
      (if (null cdr-lst)
	(nreverse product)
	(iter cdr-lst (cons (- (log (car cdr-lst)) (log (car lst))) product))))))

;;; リングバッファを使って系列から平均、分散の系列を作る
(defun mean-variance-list (lst size-of-ring-buffer)
  (let ((rb (make-ring-buffer :size size-of-ring-buffer)))
    (nlet iter
	((lst lst)
	 (mean-list nil)
	 (var-list nil))
      (if (null lst)
	(values (nreverse mean-list)
		(nreverse var-list))
	(progn
	  (rb-push! rb (car lst))
	  (iter (cdr lst)
		(cons (ring-buffer-average rb) mean-list)
		(cons (rb-variance rb) var-list)))))))
    



    
(defun multiple? (n m)
  "Return whether n is a multiple of m.
Examples:
  (multiple? 12 3) => t
  (multiple? 11 3) => nil"
  (zerop (mod n m)))

;;; DONE: lparallelに置き換え
;; ;;; cl-muprocによる並列化版のmapcar。pmapcar*は返値のリストが終わった順になる。
;; #+sbcl
;; (defun pmapcar (fn lis &key (timeout-sec 100))
;;   (labels ((gather (lis)
;;              (if (null lis) nil
;; 	       (muproc:mumsg-receive (from)
;; 		 ((value key) (eq key (car lis))
;; 		  (cons value (gather (cdr lis))))))))
;;     (muproc:muprocn (timeout-sec)
;;       (muproc:muproc-with-registered-port (:self)
;;         (gather
;;          (mapcar #'(lambda (i)
;;                      (let ((key (gensym)))
;;                        (muproc:muproc-spawn
;;                         key
;;                         #'(lambda ()
;;                             (muproc:mumsg-send :self :value (funcall fn i) :key key))
;;                         ()
;;                         :errorstream *trace-output*)
;;                        key))
;;                  lis))))))

;; #+sbcl
;; (defun pmapcar* (fn lis  &key (timeout-sec 100))
;;   (labels ((gather (n lis)
;;              (if (zerop n) lis
;;                  (muproc:mumsg-receive (from)
;;                    ((value) t
;;                     (gather (1- n) (cons value lis)))))))
;;     (muproc:muprocn (timeout-sec)
;;       (muproc:muproc-with-registered-port (:self)
;;         (gather
;;          (reduce #'(lambda (count i)
;;                      (muproc:muproc-spawn
;;                       (gensym)
;;                       #'(lambda ()
;;                           (muproc:mumsg-send :self :value (funcall fn i)))
;;                       () :errorstream *trace-output*)
;;                      (1+ count))
;;                  lis :initial-value 0)
;;          nil)))))

(defun map-tree (fn tree)
  (cond ((null tree) nil)
	((atom tree) (funcall fn tree))
	(t (cons (map-tree fn (car tree))
		 (map-tree fn (cdr tree))))))

(defun methods-of (object)
  (moptilities::specializer-direct-methods (class-of object)))

;; ハッシュテーブルのキーと値を全て表示する
(defun hash-printall (hashtable &optional (stream *standard-output*))
  (maphash #'(lambda (key value)
	       (format stream "key: ")
	       (write key :stream stream)
	       (format stream " , value: ")
	       (write value :stream stream)
	       (format stream "~%"))
	   hashtable))

;; 色々なオブジェクトのリストを文字列に連結する
(defun catstr (&rest obj-list)
  "Example:
 (catstr 'foo 'bar) => \"FOOBAR\"
 (catstr \"foo\" \"bar\") => \"foobar\""
  (reduce (lambda (s1 s2)
	    (concatenate 'string (format nil "~A" s1) (format nil "~A" s2)))
	  obj-list))

(defun catstr-list (obj-list)
  "Example:
 (catstr-list '(foo bar)) => \"FOOBAR\"
 (catstr-list '(\"foo\" \"bar\")) => \"foobar\""
  (reduce (lambda (s1 s2)
	    (concatenate 'string (format nil "~A" s1) (format nil "~A" s2)))
	  obj-list))

(defun map-plist (func plist &optional product)
  "Example:
 (map-plist (lambda (key val)
 	     (format t \"Key: ~A, Value: ~A~%\" key val)
 	     val)
 	   '(:fookey fooval :barkey barval))
 Key: FOOKEY, Value: FOOVAL
 Key: BARKEY, Value: BARVAL
 => (FOOVAL BARVAL)"
  (assert (evenp (length plist)))
  (if (null plist)
    (nreverse product)
    (map-plist func (cddr plist)
	       (cons (funcall func (car plist) (cadr plist)) product))))


;; クラス名からそのクラスに定義されているメソッドを列挙する
#+sbcl
(defun find-all-gfs (class-name)
  (remove-duplicates
   (mapcan (lambda (class)
             (copy-list (sb-mop:specializer-direct-generic-functions class)))
           (sb-mop:compute-class-precedence-list (find-class class-name)))))
