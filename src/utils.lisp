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

(defun flatten (lst &key (key #'identity))
  (mapcan (lambda (e)
            (if (listp e)
                (flatten e :key key)
                (list e)))
          (funcall key lst)))

(defun intern-upcase (name-or-symbol &optional (pkg sb-int:sane-package))
  (intern (string-upcase name-or-symbol) pkg))


(defun unfoldn (lst n)
  (reduce (lambda (acc e)
            (if (and (consp e) (not (zerop n)))  
                (append acc (apply #'list (unfoldn e (- n 1)))) 
                (append acc (list e))))
          lst
          :initial-value nil))

(defun group-list (lst predicate)
  (labels ((rec (head acc lst)
             (if lst
                 (if (or (not head)
                         (not (funcall predicate (car head) (car lst))))
                     (rec (cons (car lst) head)
                          acc
                          (cdr lst))
                     (rec (list (car lst))
                          (cons (nreverse head) acc)
                          (cdr lst)))
                 (nreverse (cons (nreverse head) acc)))))
    (rec nil nil (sort (copy-list predicate) predicate))))

(defun multi-sort (lst predicate &rest predicates)
  (labels ((rec (groups predicates)
             (if (not predicates)
                 groups
                 (mapcar (lambda (group)
                           (rec (group-list group (car predicates))
                                (cdr predicates)))
                         groups))))
    (unfoldn (rec (group-list lst predicate) predicates)
             (1+ (length predicates)))))


(defun extract-set (tree predicate &key (key #'identity) (test #'eql))
  (let ((adjoin-fn (lambda (lst e)
                     (adjoin e lst :key key :test test))))
    (reduce (lambda (acc e)
              (if (consp e)
                  (reduce adjoin-fn 
                          (extract-set e predicate :key key :test test)
                          :initial-value acc)
                  (or (and (funcall predicate e) (funcall adjoin-fn acc e))
                      acc)))
            tree
            :initial-value nil)))
