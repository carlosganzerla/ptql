(in-package #:ptql)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defmacro defglobal (name table)
  (with-gensyms (var)
    `(let ((,var ,name))
       (setf (symbol-value ,var) ,table)
       ,var)))

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun split-string (input tokens)
  (with-input-from-string (str input)
    (let ((splits nil)
          (current (make-adjustable-string "")))
      (do ((chr (read-char str nil :eof) (read-char str nil :eof)))
          ((eql chr :eof) (nreverse (push current splits)))
          (if (member chr tokens)
              (progn
                (push current splits)
                (setf current (make-adjustable-string "")))
              (vector-push-extend chr current))))))

(defun select-keys (prolst keys)
  (let ((result nil))
    (do* ((key (pop keys) (pop keys))
          (val (getf prolst key) (getf prolst key)))
      ((not key) (nreverse result))
      (when val
        (push key result)
        (push val result)))))

(defun unfold (lst)
  (reduce (lambda (acc e)
            (if (consp e)
                (append acc (apply #'list (unfold e)))
                (append acc (list e))))
          lst
          :initial-value nil))

(defun group-list (sorted-list)
  (labels ((rec (head acc lst)
             (if lst
                 (if (or (not head)
                         (equal (car head) (car lst)))
                     (rec (cons (car lst) head)
                          acc
                          (cdr lst))
                     (rec (list (car lst))
                          (cons (nreverse head) acc)
                          (cdr lst)))
                 (nreverse (cons (nreverse head) acc)))))
    (rec nil nil sorted-list)))

(defun recursive-sort (lst predicate &rest predicates)
  (labels ((rec (groups predicates)
             (if (not predicates)
                 groups
                 (mapcar (lambda (group)
                           (rec (group-list (sort group (car predicates)))
                                (cdr predicates)))
                         groups))))
    (rec (group-list (sort lst predicate)) predicates)))

(recursive-sort '((1 2)  (1 2) (1 3) (2 2) (2 1) (2 0) (1 4) (1 2) (3 3) (3 1))
                (lambda (x1 x2) (> (car x1) (car x2)))
                (lambda (x1 x2) (< (cadr x1) (cadr x2))))
