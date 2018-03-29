;;;; csharp.lisp

(in-package #:looset)

(defun recursivly-find-csharp-files (root)
  (find-files root :include-file-types '("cs")))

(defun extract-classes-from-file-by-regex (pathname)
  (loop
    :with content = (remove-multiline-comments (read-file-into-string pathname))
    :with lines = (mapcar #'remove-comments-from-line (split-text-by-line content))
    :for line :in lines
    :for matches = (cl-ppcre:all-matches-as-strings "class\\s+\\w+" line)
    :when matches
      :append matches))

(defun recursivly-extract-classes-from-file-by-regex (root)
  (loop :for file :in (recursivly-find-csharp-files root)
        :collect (list file (extract-classes-from-file-by-regex file))))

