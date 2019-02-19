;;;; looset.lisp

(in-package #:looset)

(defvar *verbose* nil)

(defvar *default-skipped-directories*
  '(".idea" ;; Idea's IDEs
    ".git"
    "pacakges") ;; .Net package
  "List of well-known directories that we usually want to skip")

(defun verbose (format &rest args)
  (when *verbose*
    (apply #'format *error-output* format args)))

(defun extract-csharp-classes-recursivly ()
  (dolist (classes-by-file (recursivly-extract-classes-from-file-by-regex (truename ".")))
    (destructuring-bind (file classes)
        classes-by-file
      (when classes
        (format t "~A;~{~A~^;~}~%" file classes)))))

(opts:define-opts
  ;; TODO actually implement that
  (:name :help
   :description "print this help"
   :short #\h
   :long "help")
  ;; TODO actually implement that
  (:name :verbose
   :description "log information to stderr"
   :short #\v
   :long "verbose"))

(defun print-help ()
  (opts:describe
   :prefix "looset: tools for code analysi"
   :usage-of "looset"
   :args "COMMANDS"
   :suffix "Available commands:
  cs                       c# tools

Usage example:
  looset cs > classes.csv # that's the only thing supported at the moment"))

(defun dispatch-command (args)
  "Call the right function based on the command-line arguments"
  (string-case:string-case ((first args))
    ("cs" (extract-csharp-classes-recursivly))
    ;; TODO this would be a nice place for fuzzy string matching, to help the user
    (t (error "Invalid command: ~a" (first args)))))

(defun main (args)
  ;; TODO catch possible conditions and print a nice message (see https://github.com/libre-man/unix-opts/example/example.lisp)
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (when (getf options :verbose)
      (setf *verbose* t))
    (verbose "~&arguments: ~a~%free arguments: ~a~%" options free-args)
    (if (getf options :help)
        (print-help)
        (dispatch-command args))))

