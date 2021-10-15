(in-package #:ptql)

(defun read-file (path)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil :eof) (read-line str nil :eof))
         (lines nil (cons line lines)))
        ((eql line :eof) (nreverse lines)))))

(defun parse-columns (columns)
  (mapcar (lambda (s)
            (if (string-equal "" s)
                (intern-symbol (gensym) t)
                (intern-symbol s t)))
          columns))

(defun parse-row (columns row)
  (let ((parsed nil))
    (mapc (lambda (key val)
            (setf parsed (append parsed (list key val))))
          columns
          row)
    parsed))

(defun parse-rows (columns rows)
  (mapcar (lambda (row) (parse-row columns row)) rows))

(defun parse-table (path name &key (tokens (list #\,)))
  (let* ((contents (read-file path))
         (cells (mapcar (lambda (r)
                          (split-string r tokens))
                        contents))
         (columns (parse-columns (car cells)))
         (rows (parse-rows columns (cdr cells))))
    (defglobal name
              (make-table :columns columns :rows rows))))
