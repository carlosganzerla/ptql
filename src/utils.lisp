(in-package #:ptql)


(defun intern-name (symb)
  (intern (symbol-name symb)))

(defun intern-keyword (name)
  (intern name "KEYWORD"))

(defun intern-global (name)
  (intern (concatenate 'string "*" name "*")))

(defun get-value (x)
  (if (symbolp x)
      (symbol-value x)
      x))

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

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

