(in-package #:ptql)

(defmacro create-row-expr (row-symbol expr)
  (let* ((row-keys (remove-if-not #'symbolp (get-value row-symbol)))
         (expr-key (find expr row-keys :key #'intern-name)))
    (cond ((consp expr)
           (cons (car expr)
                 (reduce (lambda (acc symb)
                           (subst `(getf ,row-symbol ,symb)
                                  (intern-name symb)
                                  acc))
                         row-keys
                         :initial-value (cdr expr))))
          (expr-key `(getf ,row-symbol ,expr-key))
          (t expr))))

(defmacro select (syms &key from)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym))
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name from)))))))


(defparameter *row* (list :a 1 :b 2 :c 3))
(defparameter *rows* (make-list 5 :initial-element (list :a 1 :b 2 :c 3)))
(create-row-expr *row* (or (+ a c) (> c (- b a))))
(create-row-expr *row* (> a c))
(create-row-expr nil (or (+ a c) (> c (- b a))))
(create-row-expr nil x)
(create-row-expr *row* a)
(where *rows* nil)
(let ((cols (list :a :b :c)) (row (list :a 1 :b 2 :c 3)))
  (a-where cols row (+ a 1)))



(defmacro query (from)
  `(dolist (row ,(table-rows (intern-global from)))
    (let (,@(mapcar (lambda (symb) 
                      `(,(intern-name symb) (getf ,row ,symb)))
                   (table-columns (intern-global from)) 
                    )))))

(query table)
