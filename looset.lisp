;;;; looset.lisp

(in-package #:looset)

(defvar *default-skipped-directories*
  '(".idea" ;; Idea's IDEs
    ".git"
    "pacakges") ;; .Net package
  "List of well-known directories that we usually want to skip")

(defun main (args)
  "Dummy main program. Only prints out the command line arguments."
  (dolist (file (recursivly-find-csharp-files (truename ".")))
    (print file)))
