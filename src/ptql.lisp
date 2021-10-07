(in-package #:ptql)

(defmacro create-row-expr (row expr)
  `(reduce (lambda (expr sym)
             (subst `(getf ,,row ,sym) (intern (symbol-name sym)) expr))
           (remove-if-not #'symbolp ,row)
           :initial-value ',expr))

(defmacro select (syms &key from where)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym)) 
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name from)))))))


(create-row-expr *row* (or (+ a c) (> c (- b a))))
