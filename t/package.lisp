(defpackage #:ptql/test
  (:use #:cl #:fiveam #:ptql))

(in-package #:ptql/test)

(def-suite ptql)

(defun run-tests ()
  (run! 'ptql))
