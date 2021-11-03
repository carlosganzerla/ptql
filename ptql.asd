;;;; ptql.asd

(defsystem #:ptql
  :description "Plain Text Query language"
  :author "Carlo Sganzerla <mapard.carlo@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :depends-on (#:parse-number)
  :build-pathname "bin/ptql"
  :entry-point "ptql:repl"
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
               (:file "table")
               (:file "parser")
               (:file "ptql")
               (:file "repl"))
  :in-order-to ((test-op (test-op #:ptql/test))))

(defsystem #:ptql/test
  :depends-on (#:ptql #:fiveam)
  :pathname "t/"
  :components ((:file "package")
               (:file "test")
               (:file "test2"))
  :perform (test-op (o c) (symbol-call :ptql/test :run-tests)))
