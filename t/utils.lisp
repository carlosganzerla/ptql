(in-package #:ptql/tests)

(defun test-fail (fn &rest args)
  (handler-case 
    (apply fn args)
    (condition () t)))
