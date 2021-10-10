(in-package #:ptql)

(defun print-row (row)
  (format t "~{~10a~}~%" row))

(defun print-rows (rows)
  (mapc #'print-row 
        (cons (remove-if-not #'symbolp (car rows))
              (mapcar (lambda (row) (remove-if #'symbolp row))
                      rows)))
  nil)
