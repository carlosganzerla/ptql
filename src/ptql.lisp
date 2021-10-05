(in-package #:ptql)

(defun intern-keyword (name)
  (intern name "KEYWORD"))

(defun intern-global (name)
  (intern (concatenate 'string "*" name "*")))

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

(defun read-file (path)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil :eof) (read-line str nil :eof))
         (lines nil (cons line lines)))
        ((eql line :eof) (nreverse lines)))))

(defun parse-columns (columns)
  (mapcar (lambda (s)
            (if (string-equal "" s)
                (intern-keyword (string (gensym)))
                (intern-keyword (string-upcase s))))
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

(defmacro deftable (name table)
  (with-gensyms (var)
    `(let ((,var (intern-global ,name)))
       (setf (symbol-value ,var) ,table)
       ,var)))

(defstruct table
  (columns nil :read-only t)
  (rows nil :read-only t))

(defun parse-table (path &key (tokens '(#\,)) (name path))
  (let* ((contents (read-file path))
         (cells (mapcar (lambda (r)
                          (split-string r tokens))
                        contents))
         (columns (parse-columns (car cells)))
         (rows (parse-rows columns (cdr cells))))
    (deftable (string-upcase name)
              (make-table :columns columns :rows rows))))

(defun get-keys (lst keys)
  (let ((result nil))
    (do* ((key (pop keys) (pop keys))
          (val (getf lst key) (getf lst key)))
      ((not key) (nreverse result))
      (when val
        (push key result)
        (push val result)))))

(defmacro select (syms table)
  (let ((keys (mapcar (lambda (sym) (intern-keyword sym)) 
                      (mapcar #'symbol-name syms))))
    `(mapcar (lambda (row) (get-keys row ',keys))
             (table-rows (symbol-value (intern-global ,(symbol-name table)))))))
