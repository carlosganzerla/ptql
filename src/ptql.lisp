(in-package #:ptql)

(defvar *database* (make-hash-table))

(defun find-table (table)
  (or (gethash table *database*)
      (error (format nil "Table ~A does not exist" table))))

(defun expr-vars (expr)
  (remove-duplicates (remove-if-not #'variablep (flatten expr :key #'cdr))))

(defmacro %where (table expr)
  `(table-filter
     ,table
     (lambda (row)
       ,(cond ((consp expr)
               (reduce (lambda (expr col)
                         (subst `(get-cell ,table ,col row)
                                col expr
                                :test (test-safe #'string=
                                                 #'variablep)))
                       (mapcar (rcurry #'intern-upcase :keyword)
                               (expr-vars expr))
                       :initial-value expr))
              ((variablep expr)
               `(get-cell ,table ,(intern-upcase expr :keyword) row))
              (t expr)))))

(defun %select (table symbols)
  (table-select table
                (etypecase symbols
                  (symbol (if (string= symbols '*)
                              (columns table)
                              (format-error "Invalid symbol: ~A" symbols)))
                  (list (mapcar (rcurry #'intern-upcase :keyword)
                                symbols)))))

(defmethod get-sort-fn (table col)
  (if (consp col)
      (destructuring-bind (col clause) col
        (with-col-assertion (table col)
          (values (if (string= clause :desc)
                      #'string>
                      #'string<) col)))
      (with-col-assertion (table col) (values #'string< col))))


(defun %order-by (table clauses)
  (apply (curry #'table-sort table)
         (mapcar (lambda (clause)
                   (lambda (row1 row2)
                     (multiple-value-bind (fn col) (get-sort-fn table clause)
                       (funcall fn
                                (get-cell table col row1)
                                (get-cell table col row2)))))
                 (map-atoms (rcurry #'intern-upcase :keyword) clauses))))


(defun import-table (path table)
  (multiple-value-bind (rows columns) (parse-table path)
    (setf (gethash table *database*)
          (make-instance 'table :columns columns :rows rows))
    table))


(defmacro select (symbols &key from (where t) order-by)
  `(%select (%order-by (%where (find-table ',from) ,where) ',order-by)
            ',symbols))
