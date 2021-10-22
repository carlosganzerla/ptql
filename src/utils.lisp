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
