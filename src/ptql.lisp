(in-package #:ptql)

(defmacro %where (rows expr)
  (let ((syms ))))

(defmacro select (syms &key from where)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym)) 
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (%where 
               (table-rows (symbol-value (intern-global ,(symbol-name from))))
               where))))

