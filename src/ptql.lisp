(in-package #:ptql)

(defmacro create-row-expr (row expr)
  (with-gensyms (m-row m-expr)
    `(let ((,m-row ,row) (,m-expr ',expr))
       (cons (car ,m-expr) 
             (reduce (lambda (r-expr sym)
                       (subst (getf ,m-row sym) 
                              (intern (symbol-name sym)) 
                              r-expr))
                     (remove-if-not #'symbolp ,m-row)
                     :initial-value (cdr ,m-expr))))))

(defmacro where (rows expr)
  `(remove-if-not (lambda (row) (create-row-expr row ,expr)) ,rows))

(defmacro select (syms &key from where)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym))
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name from)))))))


(let ((s (table-rows *table*)))
  (where s (eql id 1)))

(create-row-expr *row* (or (+ a c) (> c (- b a))))
(create-row-expr (list :a 1 :b 4 :c 5) (and (+ (c + (c a) b ) a) nil))
(create-row-expr nil (or (+ a c) (> c (- b a))))
