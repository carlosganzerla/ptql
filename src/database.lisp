(in-package #:ptql)

(defvar *database* (make-hash-table))

(defclass table () 
  ((columns :accessor columns
            :initarg :columns
            :iniform nil)
   (rows :accessor rows
         :initarg :rows
         :iniform nil)
   (parent :accessor parent
           :initarg :parent
           :iniform nil)))

(defmacro deftableop (op-name bindings columns-form rows-form)
  `(defmethod ,op-name ((,(car bindings) table) ,@(cdr bindings))
     (make-instance 'table
                    :columns ,columns-form
                    :rows ,rows-form
                    :parent ,(car bindings))))

(deftableop select (table columns) columns
            (mapcar (lambda (row)
                      (select-keys row columns))
                    (rows table)))

(deftableop where (table predicate) (columns table)
            (remove-if-not predicate (rows table)))

(defun default-order-by (r1 r2)
  (declare (ignore r1) (ignore r2))
  nil)

(deftableop order-by (table &rest predicates) (columns table)
            (apply #'multi-sort 
                   (list (rows table) 
                         (or predicates `(,#'default-order-by)))))

(defun find-table (table)
  (or (gethash table-name *database*)
      (error (format nil "Table ~A does not exist" table))))

(set-macro-character #\~ ())
(defun )

