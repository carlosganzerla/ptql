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


(deftableop table-sort (table &rest predicates) (columns table)
  (apply #'multi-sort 
         (cons (rows table) 
               (or predicates
                   (list (lambda (r1 r2) 
                           (declare (ignore r1) (ignore r2)) nil))))))

