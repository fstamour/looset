;;;; filesystem.lisp

(in-package #:looset)

(defun skip-directories (pathname test directories)
  "Takes a pathname, returns nil if the pathname's directory matches one of the directories."
  (if directories
      (not
       (member
        (lastcar (pathname-directory pathname))
        directories
        :test test))
    t))

(defun pathname-type-member (pathname types test)
  (member (pathname-type pathname) types :test test))

(defun find-files (root-directory
                   &key (test
                         #+windows #'string-equal
                         #-windows #'string=)
                     (skip-directories *default-skipped-directories*)
                     include-file-types)
  "Returns a list of files that matches the critera."
  (while-collecting
      (collect-file)
    (map-files root-directory
               #'(lambda (file)
                   (collect-file file))
               :test test
               :skip-directories skip-directories
               :include-file-types include-file-types)))

(defun map-files (root-directory callback
                  &key (test
                        #+windows #'string-equal
                        #-windows #'string=)
                    (skip-directories *default-skipped-directories*)
                    include-file-types)
  "Calls \"callback\" on each files that matches the critera."
  (collect-sub*directories
   root-directory
   t
   (lambda (pathname)
     (-> pathname
         (skip-directories test skip-directories)))
   #'(lambda (directory)
       (dolist (file (directory-files directory))
         (if include-file-types
             (when
                 (pathname-type-member file include-file-types test)
               (funcall callback file))
             (funcall callback file))))))

