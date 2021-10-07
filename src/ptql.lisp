(in-package #:ptql)

(defmacro create-row-expr (row expr)
  (cons (car expr) 
        (reduce (lambda (expr sym)
                  (subst `(getf ,row ,sym) (intern (symbol-name sym)) expr))
                (remove-if-not #'symbolp (coerce-symbol row))
                :initial-value (cdr expr))))

(defmacro select (syms &key from where)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym)) 
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name from)))))))


(create-row-expr *row* (or (+ a c) (> c (- b a))))
(create-row-expr (list :a 1 :b 4 :c 5) (and (+ (c + (c a) b ) a) nil))
(create-row-expr nil (or (+ a c) (> c (- b a))))
