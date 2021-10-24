(in-package #:ptql)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defun format-error (message &rest args)
  (error (apply #'format `(nil ,message ,@args))))

(defun curry (fn &rest args)
  (lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  (lambda (&rest args2)
      (apply fn (append args2 args)))) 

(defun test-safe (fn test)
  (lambda (&rest args)
    (if (every test args)
        (apply fn args)
        nil)))

(defun variablep (obj)
  (and (symbolp obj) (not (eql t obj)) (not (eql nil obj))))

(defun flatten (lst &key (key #'identity))
  (mapcan (lambda (e)
            (if (listp e)
                (flatten e :key key)
                (list e)))
          (funcall key lst)))

(defun intern-upcase (name-or-symbol &optional (pkg sb-int:sane-package))
  (intern (string-upcase name-or-symbol) pkg))

(defun map-atoms (mapping tree)
  (mapcar (lambda (e)
            (if (atom e)
                (funcall mapping e)
                (map-atoms mapping e)))
          tree))

(defun unfoldn (lst n)
  (reduce (lambda (acc _)
            (declare (ignore _))
            (mapcan #'identity acc))
          (make-list n)
          :initial-value lst))

(defun group-list (lst predicate)
  (labels ((rec (head acc lst)
             (cond ((not lst) (nreverse (cons (nreverse head) acc)))
                   ((and head (funcall predicate (car head) (car lst)))
                    (rec (list (car lst)) (cons (nreverse head) acc) (cdr lst)))
                   (t (rec (cons (car lst) head) acc (cdr lst))))))
    (rec nil nil (sort (copy-list lst) predicate))))

(defun multi-sort (lst predicate &rest predicates)
  (labels ((rec (groups predicates)
             (if predicates
                 (mapcar (lambda (group)
                           (rec (group-list group (car predicates))
                                (cdr predicates)))
                         groups)
                 groups)))
    (unfoldn (rec (group-list lst predicate) predicates)
             (1+ (length predicates)))))
