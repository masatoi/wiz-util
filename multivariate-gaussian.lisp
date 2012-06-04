;;; load utilities
(in-package :wiz-util)

;;; Gaussian ==================================================================
(defun gaussian (x mu sigma^2)
  (if (zerop sigma^2)
      least-positive-double-float
      (/ (exp (- (/ (expt (- x mu) 2)
		    (* 2 sigma^2))))
	 (sqrt (* 2 pi sigma^2)))))
      
      
  
(defun multivariate-gaussian (x mu Sigma &key (inv-Sigma nil))
  ;; TODO: inv-Sigmaを指定しないときの逆行列計算はコレスキー分解を利用するようにする
  (let ((D (array-dimension x 0))
	(inv-Sigma (if inv-Sigma inv-Sigma (m-1 Sigma))))
    (/ (exp (* (aref (m* (m-t (m- x mu)) inv-Sigma (m- x mu)) 0 0) -0.5d0))
       (sqrt (* (expt (* 2.0d0 pi) D) (det-cholesky Sigma))))))

;; 1次元と多次元に同時に対応する
(defun universal-gaussian (x mu Sigma &key (inv-Sigma nil))
  (let ((gaussian-val
	 (if (arrayp x)
	     (multivariate-gaussian x mu Sigma :inv-Sigma inv-Sigma)
	     (gaussian x mu Sigma))))
    (if (zerop gaussian-val)
	least-positive-double-float
	gaussian-val)))

;;; Sampling ==================================================================

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