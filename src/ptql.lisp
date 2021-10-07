(in-package #:ptql)

(defmacro create-row-expr (row expr)
  (let ((syms (get-symbols (cdr expr))))
    (reduce (lambda (expr sym)
              (subst `(getf ,row ,(intern-keyword (symbol-name sym))) 
                     sym
                     expr))
            syms
            :initial-value (cdr expr))))

(defmacro select (syms &key from where)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym)) 
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name from)))))))


(create-row-expr *row* (or (+ a (eql a b)) (> 0 (- b a))))
