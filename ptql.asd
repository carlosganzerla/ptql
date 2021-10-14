;;;; ptql.asd

(asdf:defsystem #:ptql
  :description "Plain Text Query language"
  :author "Carlo Sganzerla"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/table")
               (:file "src/parser")
               (:file "src/repl")
               (:file "src/ptql")))
