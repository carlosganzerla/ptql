;;;; ptql.asd

(asdf:defsystem #:ptql
  :description "Plain Text Query language"
  :author "Carlo Sganzerla"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "bin/ptql"
  :entry-point "ptql:repl"
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/table")
               (:file "src/parser")
               (:file "src/ptql")
               (:file "src/repl")))
