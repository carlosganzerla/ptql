(in-package #:ptql)

(defmacro deftable (name table)
  (with-gensyms (var)
    `(let ((,var (intern-global ,name)))
       (setf (symbol-value ,var) ,table)
       ,var)))

(defstruct table
  (columns nil :read-only t)
  (rows nil :read-only t))

