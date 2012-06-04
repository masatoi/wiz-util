;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common-Lisp; -*-

;; TODO: ファイルへの出力がうまくいっていない

(defpackage :nlisp-wrapper
  (:use :common-lisp :wiz-util)
  (:nicknames :nplot)
  (:export :plot-list :nplot-list :plot-list-2dim))

(in-package :nlisp-wrapper)

;;; setting for Gnuplot
(defvar nlisp::*gnuplot-path* "/usr/local/bin/gnuplot")
(defvar nlisp::*gnuplot-terminal-type* "x11")

;;; NLISP plot utilities =========================================================
(defun random-uniform (start end)
  (+ (random (- end start)) start))

(defun seq (start end &key (by 1))
  (nlet itf ((i end)
	     (product '()))
    (if (< i start)
	product
	(itf (- i by) (cons i product)))))

(defun list->nlisp-double (list)
  (let* ((len (length list))
	 (nlisp-list (nlisp:make-double-array len)))
    (sfor (i 0 (1- len))
      (setf (nlisp:.aref nlisp-list i) (* (nth i list) 1.0d0)))
    nlisp-list))

(defun list->nlisp-array-double (list)
  (let* ((len1 (length list))
	 (len2 (length (car list)))
	 (nlisp-array (nlisp:make-double-array (list len1 len2))))
    (sfor (i 0 (1- len1))
      (sfor (j 0 (1- len2))
	(setf (nlisp:.aref nlisp-array i j) (* (nth j (nth i list)) 1.0d0))))
    nlisp-array))

;;; NLISP plot functions =========================================================

(defun plot-list (y-list
		 &key (x-list nil) (title " ") (xlabel " ") (ylabel " ")
		 (style "lines")  (reset t) (aspect-ratio 1.0)
		 (output-file nil) (output-format :png)
		 (xrange nil) (yrange nil) (xrange-reverse nil) (yrange-reverse nil)
		 (key t))

  (if reset (nlisp:reset-plot)) ; ここでリセットするのでplotの呼び出しでは :reset nil とする
  
  (if output-file
      (ecase output-format
	(:eps (setf nlisp::*gnuplot-terminal-type* "postscript eps enhanced color")
	      (format (nlisp::gplot-stream) "set output \"~A\"~%" output-file))
	(:png (setf nlisp::*gnuplot-terminal-type* "png")
	      (format (nlisp::gplot-stream) "set output \"~A\"~%" output-file)))
      (setf nlisp::*gnuplot-terminal-type* "x11"))

  (if xrange
      (format (nlisp::gplot-stream) "set xrange [~A:~A] " (car xrange) (cadr xrange))
      (format (nlisp::gplot-stream) "set xrange [] "))
  (if xrange-reverse
      (format (nlisp::gplot-stream) "reverse"))
  (format (nlisp::gplot-stream) "~%")

  (if yrange
      (format (nlisp::gplot-stream) "set yrange [~A:~A] " (car yrange) (cadr yrange))
      (format (nlisp::gplot-stream) "set yrange [] "))
  (if yrange-reverse
      (format (nlisp::gplot-stream) "reverse"))
  (format (nlisp::gplot-stream) "~%")

  (if (null key)
      (nlisp:plot-command "unset key~%"))

  (nlisp:plot (list->nlisp-double y-list)		  
		  (if x-list (list->nlisp-double x-list) (nlisp:.iseq 0 (1- (length y-list))))
		  :title title :xlabel xlabel :ylabel ylabel
		  :style style :reset nil :aspect-ratio aspect-ratio))

(defun nplot-list (y-list
		  &key (x-list nil) (title " ") (xlabel " ") (ylabel " ")
		  (style "lines")  (reset t) (aspect-ratio 1.0)
		  (output-file nil) (output-format :png)
		  (xrange nil) (yrange nil) (xrange-reverse nil) (yrange-reverse nil) (key t))

  (if reset (nlisp:reset-plot)) ; ここでリセットするのでplotの呼び出しでは :reset nil とする

  (if output-file
      (ecase output-format
	(:eps (setf nlisp::*gnuplot-terminal-type* "postscript eps enhanced color")
	      (format (nlisp::gplot-stream) "set output \"~A\"~%" output-file))
	(:png (setf nlisp::*gnuplot-terminal-type* "png")
	      (format (nlisp::gplot-stream) "set output \"~A\"~%" output-file)))
      (setf nlisp::*gnuplot-terminal-type* "x11"))
  
  (if xrange
      (format (nlisp::gplot-stream) "set xrange [~A:~A] " (car xrange) (cadr xrange))
      (format (nlisp::gplot-stream) "set xrange [] "))
  (if xrange-reverse
      (format (nlisp::gplot-stream) "reverse"))
  (format (nlisp::gplot-stream) "~%")

  (if yrange
      (format (nlisp::gplot-stream) "set yrange [~A:~A] " (car yrange) (cadr yrange))
      (format (nlisp::gplot-stream) "set yrange [] "))
  (if yrange-reverse
      (format (nlisp::gplot-stream) "reverse"))
  (format (nlisp::gplot-stream) "~%")

  (if (null key)
      (nlisp:plot-command "unset key~%"))

  (nlisp:nplot (mapcar #'list->nlisp-double y-list)
	       (if x-list (list->nlisp-double x-list) (nlisp:.iseq 0 (1- (length (car y-list)))))
	       :title title :xlabel xlabel :ylabel ylabel
	       :styles (make-list (length y-list) :initial-element style) :reset nil :aspect-ratio aspect-ratio))
  
;; ex: 1dimension gaussian
;; (plot-list (mapcar (lambda (x) (gaussian x -1 0.3)) (seq -2 2 :by 0.1d0)) (seq -2 2 :by 0.1d0))

(defun plot-list-2dim (z-func x-list y-list
		      &key (title "Image Plot") (xlabel "x") (ylabel "y")
		      (surface t) (output-file nil) (output-format :png) (reset t))
  (nlisp:palette-defined '((0.0 "#0000FF") (0.5 "#00AAFF") (1.0 "#00FFFF")
			   (1.5 "#00FFAA") (2.0 "#00FF00") (2.5 "#AAFF00")
			   (3.0 "#FFFF00") (3.5 "#FFAA00") (4.5 "#FF0000")
			   (5.0 "#FF00AA") (6.0 "#FF00FF")))

  (if reset (nlisp:reset-plot)) ; ここでリセットするのでplotの呼び出しでは :reset nil とする
  (nlisp:plot-command "set size 1.0")

  (if output-file
      (ecase output-format
	(:eps (setf nlisp::*gnuplot-terminal-type* "postscript eps enhanced color")
	      (format (nlisp::gplot-stream) "set output \"~A\"~%" output-file))
	(:png (setf nlisp::*gnuplot-terminal-type* "png")
	      (format (nlisp::gplot-stream) "set output \"~A\"~%" output-file)))
      (setf nlisp::*gnuplot-terminal-type* "x11"))
  
  (nlisp::image-pm3d (list->nlisp-array-double (mapcar
					    (lambda (y)
					      (mapcar
					       (lambda (x) (funcall z-func x y))
					       x-list))
					    y-list))
		 (list->nlisp-double x-list)
		 (list->nlisp-double y-list)
		 :title title :xlabel xlabel :ylabel ylabel
		 :reset nil :surface surface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; histogram ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 区間(a,b)をn分割したとき,xがどの区間に入るかを返す
(defun histogram-lem1 (x a b n)
  (if (or (< x a) (< b x))
      nil
      (if (= x b)
	  (1- n)
	  (let ((span (/ (- b a) n)))
	    (nlet itr ((i 1))
	      (if (<= x (+ a (* i span)))
		  (1- i)
		  (itr (1+ i))))))))

;; サンプルのリストsamplesをrange幅で分割し,それぞれの区間での登場回数を数え上げる.
;; 各区間のサンプル登場回数のリストを返す.
(defun histogram (samples n-of-bin)  
  (let* ((counter (make-list n-of-bin :initial-element 0))
	 (a (apply #'min samples))
	 (b (apply #'max samples))
	 (span (/ (- b a) n-of-bin)))
    (dolist (x samples)
      (let ((bin (histogram-lem1 x a b n-of-bin)))
	(if bin (incf (nth bin counter)))))
    (loop for i from 0  to (1- n-of-bin) do
	 (format t "~$ - ~$: ~A~%"
		 (+ a (* i span))
		 (+ a (* (1+ i) span))
		 (nth i counter)))
    (plot-list counter
	      :x-list (loop for i from 0  to (1- n-of-bin) collect (+ a (* i span)))
	      :style "impulse")))

;; ヒストグラムをgnuplotへの入力形式で出力する
(defun output-histogram (path sample-list a b n)
  (let ((list (histogram sample-list n)))
    (with-open-file (f path :direction :output :if-exists :supersede)
      (let ((L (/ (- b a) n)))
	(sfor (i 0 (- n 1))
	  (format f "~A ~A~%" (+ a (* i L)) (nth i list)))))))