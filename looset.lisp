;;;; looset.lisp

(in-package #:looset)

(defvar *default-skipped-directories*
  '(".idea" ;; Idea's IDEs
    ".git"
    "pacakges") ;; .Net package
  "List of well-known directories that we usually want to skip")

(defun main (args)
  (dolist (classes-by-file (recursivly-extract-classes-from-file-by-regex (truename ".")))
    (destructuring-bind (file classes)
        classes-by-file
      (when classes
        (format t "~A;~{~A~^;~}~%" file classes)))))
