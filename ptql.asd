;;;; ptql.asd

(asdf:defsystem #:ptql
  :description "Plain Text Query language"
  :author "Carlo Sganzerla"
  :license  "MIT"
  :version "0.0.1"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "ptql")))
