(in-package #:ptql)

(defun intern-keyword (name-or-symb)
  (intern (string-upcase name-or-symb) "KEYWORD"))

(defun intern-global (name-or-symb)
  (intern (concatenate 'string "*" (string-upcase name-or-symb) "*")))

(defstruct table
  (columns nil :read-only t)
  (rows nil :read-only t))

(defun find-table (name-or-symb)
  (symbol-value (intern-global name-or-symb)))
