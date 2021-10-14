(in-package #:ptql)

(defun print-row (row)
  (format t "~{~10a~}~%" row))

(defun print-rows (rows)
  (mapc #'print-row 
        (cons (remove-if-not #'symbolp (car rows))
              (mapcar (lambda (row) (remove-if #'symbolp row))
                      rows)))
  nil)

(defun print-line (msg &rest args)
  (apply #'format 
         (append (list *query-io* (concatenate 'string "~&" msg "~%")) args)))

(defun read-command ()
  (read-from-string (concatenate 'string "(" (read-line *query-io*) ")")))

(defun repl ()
  (print-line "Welcome to PTQL, enter your commands or q to quit!")
  (do ((expr (read-command) (read-command)))
      ((eql (car expr) 'q) (print-line "Goodbye"))
      (case (car expr) 
        (import-table (print-line "Table ~A imported successfully" 
                                  (apply #'import-table (cdr expr))))
        (select 
          (declaim (sb-ext:muffle-conditions style-warning))
          (print-rows (eval expr)))
          (declaim (sb-ext:unmuffle-conditions style-warning))
        (t (print-line "Unknown expression: ~A" expr)))))
