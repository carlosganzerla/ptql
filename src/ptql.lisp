(in-package #:ptql)

(defvar *database* (make-hash-table))

(defun find-table (table)
  (or (gethash table *database*)
      (error (format nil "Table ~A does not exist" table))))

(defun import-table (path table)
  (multiple-value-bind (rows columns) (parse-table path)
    (setf (gethash table *database*)
          (make-instance 'table :columns columns :rows rows))))

(defun expr-vars (expr)
  (remove-duplicates (remove-if-not #'symbolp (flatten expr :key #'cdr))))

(defmacro %where (table expr)
  `(where ,table
          (lambda (row)
            ,(reduce (lambda (expr col)
                       (subst `(get-cell ,table ,col row)
                              col expr :test (test-safe #'string= #'symbolp)))
                     (mapcar (rcurry #'intern-upcase :keyword)
                             (expr-vars expr))
                     :initial-value expr))))

(defmacro %select (symbols table)
  (with-gensyms (result)
    `(let ((,result ,table))
       (select ,result
               (etypecase ',symbols
                 (symbol (if (string= ',symbols '*)
                             (columns ,result)
                             (format-error "Invalid symbol: ~A")))
                 (list (mapcar (rcurry #'intern-upcase :keyword)
                               ',symbols)))))))

(defun %order-by (table clauses)
  (apply (curry #'order-by table) 
           (mapcar (lambda (clause)
                     (lambda (row1 row2)
                       (multiple-value-bind (fn col) (get-sort-fn table clause)
                         (funcall fn
                                  (get-cell table col row1) 
                                  (get-cell table col row2)))))
                   (map-atoms (rcurry #'intern-upcase :keyword) clauses))))

(defmacro select-command (symbols &key from (where t) order-by)
  `(%select ,symbols 
            (%order-by (%where (find-table ',from) ,where) ,order-by)))


(import-table #p"~/Documents/myfile.csv" 'table)

(rows (find-table 'table))
(rows (%order-by (find-table 'table) '((name desc) age)))
