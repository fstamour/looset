;;;; package.lisp

(defpackage #:looset
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:anaphora
        #:split-sequence)
  (:import-from #:uiop
                #:while-collecting
                #:collect-sub*directories
                #:directory-files)
  (:export #:main))
