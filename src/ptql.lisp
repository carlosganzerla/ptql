(in-package #:ptql)

(defvar *database* (make-hash-table))

(defun find-table (table)
  (or (gethash table *database*)
      (error (format nil "Table ~A does not exist" table))))

(defun expr-vars (expr)
  (remove-duplicates (remove-if-not #'variablep (flatten expr :key #'cdr))))

(defun %where (table expr)
  (table-filter
    table
    (compile (gensym)
             `(lambda (row)
                ,(cond ((consp expr)
                        (reduce (lambda (expr col)
                                  (subst `(get-cell ,table ,col row)
                                         col expr
                                         :test
                                         (test-safe #'string=
                                                    #'variablep)))
                                (mapcar #'internkw
                                        (expr-vars expr))
                                :initial-value expr))
                       ((variablep expr)
                        `(get-cell ,table ,(internkw expr) row))
                       (t expr))))))

(defun %select (table symbols)
  (table-select table
                (etypecase symbols
                  (symbol (if (string= symbols '*)
                              (columns table)
                              (format-error "Invalid symbol: ~A" symbols)))
                  (list (mapcar #'internkw symbols)))))

(defun create-sort-fn (number-fn string-fn)
  (lambda (fst snd)
    (cond ((and fst (not snd)) t)
          ((and (not fst) snd) nil)
          ((and (numberp fst) (numberp snd)) (funcall number-fn fst snd))
          ((and (stringp fst) (stringp snd)) (funcall string-fn fst snd))
          (t nil))))


(defun .> (fst snd)
  (funcall (create-sort-fn #'> #'string>) fst snd))

(defun .< (fst snd)
  (funcall (create-sort-fn #'< #'string<) fst snd))

(defun get-sort-fn (table col)
  (if (consp col)
      (destructuring-bind (col clause) col
        (with-col-assertion (table col)
                            (values (if (string= clause :desc)
                                        #'.>
                                        #'.<) col)))
      (with-col-assertion (table col) (values #'.< col))))


(defun %order-by (table clauses)
  (apply (curry #'table-sort table)
         (mapcar (lambda (clause)
                   (lambda (row1 row2)
                     (multiple-value-bind (fn col) (get-sort-fn table clause)
                       (funcall fn
                                (get-cell table col row1)
                                (get-cell table col row2)))))
                 (map-atoms #'internkw clauses))))


(defun import-table (path table &key (number-coercion t))
  "Parses a file from PATH and saves it on the *DATABASE with the symbol named
   TABLE. Every column which all rows are a number will be parsed automatically
   to numeric values, unless NUMBER-COERCION is set to NIL"
  (multiple-value-bind (rows columns) (parse-table path number-coercion)
    (setf (gethash table *database*)
          (make-instance 'table :columns columns :rows rows))
    table))


(defun select (symbols &key from (where t) order-by limit)
  "Queries the table symbol on *DATABASE* specified on FROM, selecting only the
   specified SYMBOLS (columns). If * is set as SYMBOLS, all columns are
   selected. A predicate may be supplied by the keyword parameter WHERE. The
   columns must be referred as symbols. ORDER-BY defines the sorting clause.
   LIMIT will dictate the number of rows returned. If NIL, all rows will be
   returned."
  (funcall (if limit (rcurry #'table-limit limit) #'identity)
           (%select (%order-by (%where (find-table from) where)
                               order-by)
                    symbols)))
