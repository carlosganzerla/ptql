(in-package #:ptql)

(defmacro deftable (name table)
  (with-gensyms (var)
    `(let ((,var (intern-global ,name)))
       (setf (symbol-value ,var) ,table)
       ,var)))

(defstruct table
  (columns nil :read-only t)
  (rows nil :read-only t))

(defun get-select-keys (symbols)
  (mapcar #'intern-keyword (mapcar #'symbol-name symbols)))

(defun select-keys (lst keys)
  (let ((result nil))
    (do* ((key (pop keys) (pop keys))
          (val (getf lst key) (getf lst key)))
      ((not key) (nreverse result))
      (when val
        (push key result)
        (push val result)))))


(defun select-symbols (symbols rows)
  (select-keys rows (get-select-keys symbols)))

