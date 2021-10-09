(in-package #:ptql)

(defun intern-name (symb)
  (intern (symbol-name symb)))

(defun intern-keyword (name)
  (intern name "KEYWORD"))

(defun intern-global (name)
  (intern (concatenate 'string "*" name "*")))

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defmacro coerce-symbol (value)
  (with-gensyms (fallback)
    `(if (symbolp ,value)
         (symbol-value ,value)
         (let ((,fallback ,value))
           ',fallback))))

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

(defun get-keys (lst keys)
  (let ((result nil))
    (do* ((key (pop keys) (pop keys))
          (val (getf lst key) (getf lst key)))
      ((not key) (nreverse result))
      (when val
        (push key result)
        (push val result)))))
