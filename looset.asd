;;;; looset.asd

(asdf:defsystem #:looset
  :description "Tools to analyse and change code"
  :author "Francis St-Amour"
  :license  "BSD-2-clauses"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "looset")))
