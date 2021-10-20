(in-package #:ptql)

(defun split-string (str &optional (separator #\,))
  (labels ((rec (str acc)
             (let ((n (position separator str :from-end t :test #'char=)))
               (if n
                   (rec (subseq str 0 n) (cons (subseq str (1+ n)) acc))
                   (cons str acc)))))
    (rec str nil)))

(defun %read-file (path)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil :eof) (read-line str nil :eof))
         (lines nil (cons line lines))
         (len 0 (+ len 1)))
        ((eql line :eof)
         (subst nil "" (mapcar #'split-string (nreverse lines))
                :test (atom-test #'string=))))))

(defun to-keyword (name-or-symbol)
  (intern (string-upcase name-or-symbol) :keyword))

(defun %parse-columns (columns)
  (mapcar (lambda (col)
            (and col (to-keyword col)))
          columns))

(defun %parse-rows (columns rows)
  (mapcar (lambda (row) 
            (mapcan (lambda (col row)
                      (and col (list col row)))
                    columns row))
          rows))

(defun parse-table (path)
  (let* ((cells (%read-file path))
         (columns (%parse-columns (car cells)))
         (rows (%parse-rows columns (cdr cells))))
    (values rows (remove-if-not #'identity columns))))
