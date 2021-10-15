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

(defun help-error-message ()
  (print-line "Commands must be on the following form:")
  (print-line "parse-table #p\"<csv-path>\" <table-name>")
  (print-line "select ([column]* | *) :from <table-name>")
  (print-line "[:where <condition>] [:order-by ([column | (column :desc)]*)]"))

(defun read-command ()
  (print-line "Enter command:")
  (or (ignore-errors (read-from-string (concatenate 'string 
                                                    "(" 
                                                    (read-line *query-io*) 
                                                    ")")))
      (print-line "Invalid command syntax. Commands begin with a word.")))

(defun interpret-command (command)
  (handler-case
   (case (car command) 
    (help (help-error-message))
    (parse-table (print-line "Table ~A imported successfully" 
                              (apply #'parse-table (cdr command))))
    (select (without-style-warnings (print-rows (eval command))))
    (t (print-line "Unknown command ~A" command)))
   (condition (c) (print-line "~A" c))))


(defun repl ()
  (in-package #:ptql)
  (print-line "Welcome to PTQL, enter your commands or q to quit!")
  (do ((command (read-command) (read-command)))
      ((eql (car command) 'q) (print-line "Goodbye"))
      (when command (interpret-command command))))
