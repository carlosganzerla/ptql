(in-package #:ptql)

(defun get-select-keys (symbols)
  (mapcar (lambda (s) (intern-symbol s t))
          (mapcar #'symbol-name symbols)))

(defun %select (rows symbols)
  (mapcar (lambda (row)
            (select-keys row (get-select-keys symbols)))
          rows))

(defmacro import-table (filename symb)
  `(parse-table ,filename ',(intern-global symb)))

(defmacro %row-scope (table row &body body)
  `(let (,@(mapcar (lambda (symb) 
                     `(,(intern-symbol symb) (getf ,row ,symb)))
                   (table-columns (find-table table))))
     ,@body))

(defmacro %where (table expr)
  `(remove-if-not (lambda (row) (%row-scope ,table row ,expr))
                  (table-rows ,(find-table table))))

(defmacro select (symbols &key from (where t))
  `(%select (%where ,from ,where) ',symbols))
