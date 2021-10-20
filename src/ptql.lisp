(in-package #:ptql)

(defun get-select-keys (symbols)
  (mapcar (lambda (s) (intern-symbol s t))
          (mapcar #'symbol-name symbols)))

(defun %select (rows symbols)
  (mapcar (lambda (row)
            (select-keys row (get-select-keys symbols)))
          rows))

(defmacro %row-scope (table row &body body)
  (let* ((columns (table-columns (find-table table)))
         (column-symbols (mapcar #'intern-symbol columns)))
    `(let (,@(mapcar (lambda (symb) 
                       `(,symb (getf ,row ,(intern-symbol symb t))))
                     (extract-set body 
                                  (lambda (c) (member c column-symbols)))))
       ,@body)))

(defmacro %where (table expr)
  `(remove-if-not (lambda (row) (%row-scope ,table row ,expr))
                  (table-rows ,(find-table table))))

(defun get-sort-fn (col)
  (if (consp col)
      (if (eql (cadr col) :desc)
          (values 'string> (car col))
          (values 'string< (car col)))
      (values 'string< col)))

(defmacro %order-by (cols table)
  (if cols
      `(multi-sort 
         (copy-list ,rows)
         ,@(mapcar (lambda (col)
                     (multiple-value-bind (fn col-name) (get-sort-fn col)
                       `(lambda (row1 row2) 
                          (,fn 
                            (getf row1 ,(intern-symbol col-name t))
                            (getf row2 ,(intern-symbol col-name t))))))
                   cols))
      rows))

(defmacro select (symbols &key from (where t) order-by)
  `(%select (%order-by ,order-by (%where ,from ,where)) 
            (if (eql ',symbols '*)
                 (table-columns (find-table ',from))
                 ',symbols)))
