(in-package #:ptql)

(defun print-row (row)
  (format t "~{~10a~}~%" row))

(defun print-rows (rows)
  (mapc #'print-row 
        (cons (remove-if-not #'symbolp (car rows))
              (mapcar (lambda (row) (remove-if #'symbolp row))
                      rows)))
  nil)

(defun is-car (item lst) 
  (and (consp lst) (eql (car lst) item)))

(defun repl ()
  (format t "Welcome to PTQL, enter your commands or q to quit!~%")
  (do ((expr (read) (read))) 
      ((eql expr 'q) (format t "Goodbye!"))
      (cond ((is-car expr 'import-table) (eval expr))
            ((is-car expr 'select) (print-rows (eval expr)))
            (t (format t "Unknonw expression: ~A~%" expr)))))
