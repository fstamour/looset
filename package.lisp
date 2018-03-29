;;;; package.lisp

(defpackage #:looset
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:anaphora)
  (:import-from #:uiop
                #:while-collecting
                #:collect-sub*directories
                #:directory-files)
  (:export #:main))
