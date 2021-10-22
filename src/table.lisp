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


(defmacro deftableop (op-name bindings columns-form rows-form)
  `(defmethod ,op-name ((,(car bindings) table) ,@(cdr bindings))
     (make-instance 'table
                    :columns ,columns-form
                    :rows ,rows-form
                    :parent ,(car bindings))))


(deftableop select (table &optional columns) columns
            (mapcar (lambda (row)
                      (mapcan (lambda (col)
                                (list col (getf row col)))
                              (or columns (columns table))))
                    (rows table)))

(deftableop where (table predicate) (columns table)
            (remove-if-not predicate (rows table)))


(deftableop order-by (table &rest predicates) (columns table)
            (apply #'multi-sort 
                   (list (rows table) 
                         (or predicates
                             (lambda (r1 r2) 
                               (declare (ignore r1) (ignore r2)) nil)))))
