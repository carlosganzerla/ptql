(in-package #:ptql)

(defmacro select (syms table)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym)) 
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name table)))))))
