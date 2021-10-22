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
            ))

;;; NEW
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
                       (subst `(if (member ,col row)
                                   (getf row ,col)
                                   (format-error "Column ~A doesn't exist"
                                                 ,col))
                              col expr :test (test-safe #'string= #'symbolp)))
                     (mapcar (rcurry #'intern-upcase :keyword)
                             (expr-vars expr))
                     :initial-value expr))))

(%where taibol (and (> x 3) (= y 4)))

(defmacro %select (symbols table)
  (with-gensyms
    (result)
    `(let ((,result ,table))
      (select ,result
              (etypecase ',symbols
                (symbol (if (string= ',symbols '*)
                            (columns ,result)
                            (format-error "Invalid symbol: ~A")))
                (list (mapcar (rcurry #'intern-upcase :keyword)
                              ',symbols)))))))

(defmacro select-command (symbols &key from (where t))
  `(%select ,symbols (%where (find-table ',from) ,where)))

(rows (select-command *
                :from table
                :where (string> vegetable "corn")))

(%where taibol (and (> x 3) (= y 4)))

(let ((x (gethash 'table *database*)))
  (rows (%where x (or (string= vegetable "vegetable")
                      (string= celery green))))
  (rows (%select (:celery :pinto) x)))
