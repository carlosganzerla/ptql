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

(defun print-line (msg &rest args)
  (format *query-io* (concatenate 'string "~&" msg "~%") args))

(defun read-command ()
  (read-from-string (concatenate 'string "(" (read-line *query-io*) ")")))

(defun repl ()
  (print-line "Welcome to PTQL, enter your commands or q to quit!")
  (do ((expr (read-command) (read-command)))
      ((equal (car expr) 'q) (print-line "Goodbye"))
      (cond ((is-car 'import-table expr) 
             (print-line "Table ~A imported successfully" 
                         (apply #'import-table (cdr (print expr)))))
            ((is-car 'select expr) 
             (print-rows (eval expr)))
            (t (print-line "Unknown expression: ~A" expr)))))

