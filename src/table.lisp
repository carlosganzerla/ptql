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

(defmethod get-sort-fn (table col)
  (if (consp col)
      (destructuring-bind (col clause) col
        (with-col-assertion (table col) 
          (values (if (string= clause :desc)
                      #'string>
                      #'string<) col)))
      (with-col-assertion (table col) (values #'string< col))))

(deftableop select (table &optional columns) columns
  (mapcar (lambda (row)
            (mapcan (lambda (col)
                      (list col (get-cell table row col)))
                    (or columns (columns table))))
          (rows table)))

(deftableop where (table predicate) (columns table)
  (remove-if-not predicate (rows table)))


(deftableop order-by (table &rest predicates) (columns table)
  (apply #'multi-sort 
         (cons (rows table) 
               (or (print predicates)
                   (lambda (r1 r2) 
                     (declare (ignore r1) (ignore r2)) nil)))))

