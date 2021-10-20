(in-package #:ptql)

(defun read-file (path)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil :eof) (read-line str nil :eof))
         (lines nil (cons line lines))
         (len 0 (+ len 1)))
        ((eql line :eof) (values (nreverse lines) len)))))

(defun split-string (str &optional (separator #\ ))
  (labels ((rec (str acc)
             (let ((n (position separator str :from-end t :test #'char=)))
               (if n
                   (rec (subseq str 0 n) (cons (subseq str (1+ n)) acc))
                   (cons str acc)))))
    (rec str nil)))

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
    (defglobal (intern-global name)
               (make-table :columns columns :rows rows))))
