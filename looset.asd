;;;; looset.asd

(asdf:defsystem #:looset
  :description "Tools to analyse and change code"
  :author "Francis St-Amour"
  :license  "BSD-2-clauses"
  :version "0.0.1"
  :depends-on (#:trivial-features
               #:uiop
               #:alexandria
               #:anaphora
               #:cl-arrows
               #:split-sequence
               #:cl-ppcre
               #:unix-opts
               #:string-case)
  :serial t
  :components ((:file "package")
               (:file "filesystem")
               (:file "string")
               (:file "csharp")
               (:file "looset")))
