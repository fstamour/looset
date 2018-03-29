;;;; string.lisp

(in-package #:looset)

(defun remove-multiline-comments (text)
  (cl-ppcre:regex-replace-all "/\\*.*\\*/" text ""))

(defun remove-comments-from-line (line)
  (cl-ppcre:regex-replace-all "//.*$" line ""))

(defun newline-p (character)
  (member character '(#\Newline #\Return)))

(defun split-text-by-line (text)
  (split-sequence-if #'newline-p text :remove-empty-subseqs t))

