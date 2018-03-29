;;;; csharp.lisp

(in-package #:looset)

(defun recursivly-find-csharp-files (root)
  (find-files root :include-file-types '("cs")))

