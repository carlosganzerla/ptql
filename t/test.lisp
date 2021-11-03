(in-package #:ptql/test)

(def-suite pimba :in ptql)
(in-suite pimba)

(defun add1 (x) (1+ x))

(test add1-adds-one
  (is (= 3 (add1 2))))
