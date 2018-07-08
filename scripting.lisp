(in-package :wiz-util)

;; Scripting
(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun format-directory (p)
  (assert (eq (car (pathname-directory p)) :absolute))
  (reduce (lambda (a b) (cat a b "/"))
          (cons "/" (cdr (pathname-directory p)))))

(defun format-filename (p)
  (if (pathname-type p)
    (format nil "~A.~A" (pathname-name p) (pathname-type p))
    (format nil "~A"    (pathname-name p))))

(defun format-pathname (p)
  (let ((filename (format-filename p)))
    (if filename
        (cat (format-directory p) (format-filename p))
        (format-directory p))))

(defun pwd (&optional relative-path)
  (if relative-path
    (merge-pathnames relative-path (uiop:getcwd))
    (uiop:getcwd)))

(defun ls (&optional dir)
  (let ((dir (if dir dir (pwd))))
    (append (uiop:subdirectories dir)
            (uiop:directory-files dir))))

(defun cd (dir)
  (let ((result (uiop:chdir dir)))
    (if (zerop result)
      (let ((newdir (pwd)))
        (setf *default-pathname-defaults* newdir)
        newdir)
      nil)))

(defun mkdir (dir)
  (ensure-directories-exist p))

(defun rm (path)
  (uiop:delete-file-if-exists p))

(defun rm-r (dir)
  (uiop:delete-directory-tree dir))

(defun cp (in out)
  (uiop:copy-file in out))

(defun mv (src target)
  (uiop:rename-file-overwriting-target src target))

(defun exec (command &rest args)
  (uiop:run-program (cons command args)
                    :output *standard-output*
                    :error-output *standard-output*))
