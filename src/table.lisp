(in-package #:ptql)

(defclass table ()
  ((columns :accessor columns
            :initarg :columns
            :initform nil)
   (rows :accessor rows
         :initarg :rows
         :initform nil)
   (parent :accessor parent
           :initarg :parent
           :initform nil)))

(defmacro deftableop (op-name bindings columns-form &body rows-body)
  `(defmethod ,op-name ((,(car bindings) table) ,@(cdr bindings))
     (make-instance 'table
                    :columns ,columns-form
                    :rows  (progn ,@rows-body)
                    :parent ,(car bindings))))


(defmacro with-col-assertion ((table col) &body body)
  (with-gensyms (table-val)
    `(let ((,table-val ,table))
       (if (member ,col (columns ,table))
           (progn ,@body)
           (format-error "Column ~A does not exist on table ~A"
                         ,col
                         ,table-val)))))

(defmethod get-cell ((table table) col row)
  (with-col-assertion (table col)
    (getf row col)))


(deftableop table-select (table &optional (columns (columns table))) columns
  (mapcar (lambda (row)
            (mapcan (lambda (col)
                      (list col (get-cell table col row)))
                    columns))
          (rows table)))

(deftableop table-filter (table predicate) (columns table)
  (remove-if-not predicate (rows table)))

(deftableop table-limit (table count) (columns table)
  (let ((table-rows (rows table)))
   (if (< count (length table-rows))
     (subseq table-rows 0 count)
     table-rows)))

(deftableop table-sort (table &rest predicates) (columns table)
  (apply #'multi-sort
         (cons (rows table)
               (or predicates
                   (list (lambda (r1 r2)
                           (declare (ignore r1) (ignore r2)) nil))))))

(defmethod table-print ((table table) &optional (cell-length 20))
  (declare (type integer cell-length))
  (flet ((print-row (row)
           (print-line (concat "~{~" (write-to-string (+ 4 cell-length)) "A~}")
                       (mapcar (lambda (cell)
                                 (if (> (length cell) cell-length)
                                     (concat
                                       (subseq cell 0 (- cell-length 3)) "...")
                                     cell))
                               (mapcar #'to-string row)))))
    (print-row (mapcar #'symbol-name (columns table)))
    (mapcar #'print-row (mapcar (curry #'remove-if #'variablep) (rows table)))
    nil))
