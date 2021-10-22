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
                       (subst `(if (member ,col row)
                                   (getf row ,col)
                                   (format-error "Column ~A doesn't exist"
                                                 ,col))
                              col expr :test (test-safe #'string= #'symbolp)))
                     (mapcar (rcurry #'intern-upcase :keyword)
                             (expr-vars expr))
                     :initial-value expr))))

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
