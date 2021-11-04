(in-package #:ptql)

(defun split-string (str &optional (separator #\ ))
  (labels ((rec (str acc)
             (let ((n (position separator str :from-end t :test #'char=)))
               (if n
                   (rec (subseq str 0 n) (cons (subseq str (1+ n)) acc))
                   (cons str acc)))))
    (rec str nil)))

(defun read-cells (lines separator)
  (nsubst-if nil (lambda (str)
                  (and (stringp str)
                       (or (string= str "") (string-equal str "NULL"))))
            (mapcar (lambda (line)
                      (split-string (string-trim '(#\return #\ ) line) 
                                    separator))
                    lines)))

(defun read-file (path separator)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil :eof) (read-line str nil :eof))
         (lines nil (cons line lines)))
        ((eql line :eof) (read-cells (nreverse lines) separator)))))

(defun parse-columns (columns)
  (remove-if-not #'identity
                 (mapcar (lambda (col)
                           (and col (internkw col)))
                         columns)))

(defun parse-rows (columns rows)
  (mapcar (lambda (row)
            (mapcan (lambda (col row)
                      (list col row))
                    columns row))
          rows))

(defun coerce-to-number (columns rows)
  (symbol-macrolet ((cell (getf row col)))
    (reduce (lambda (rows col)
              (let ((rows-copy (copy-tree rows)))
                (dolist (row rows-copy rows-copy)
                  (aif (and cell (parse-number cell))
                    (setf cell it)
                    (and cell (return-from nil rows))))))
            columns
            :initial-value rows)))

(defun parse-file (path &optional (separator #\,) (number-coercion t))
  (let* ((cells (read-file path separator))
         (cols (parse-columns (car cells)))
         (rows (parse-rows cols (cdr cells))))
    (if number-coercion
        (values (coerce-to-number cols rows) cols)
        (values rows cols))))
