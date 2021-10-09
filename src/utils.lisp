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

