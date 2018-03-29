
(cl:in-package #:cl-user)

(ql:quickload '#:looset)

(defun save-executable (name)
  (setf uiop:*image-entry-point*
        #'(lambda ()
            (looset:main (uiop:command-line-arguments))))
  (setf uiop:*lisp-interaction* t)
  (uiop:dump-image (concatenate 'string name #+windows ".exe") :executable t))

(save-executable "looset")

