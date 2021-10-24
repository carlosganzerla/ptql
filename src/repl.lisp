(in-package #:ptql)

(defun help ()
  (print-line "Commands must be on the following form:")
  (print-line "import-table #p\"<csv-path>\" <table-name>")
  (print-line "select ([column]* | *) :from <table-name>")
  (print-line "[:where <condition>] [:order-by ([column | (column :desc)]*)]"))

(defun read-command ()
  (let ((*read-eval* nil))
    (print-line "Enter command:")
    (or (ignore-errors (read-from-string (concatenate 'string
                                                      "("
                                                      (read-line *query-io*)
                                                      ")")))
        (print-line "Invalid command syntax!"))))

(defun interpret-command (command)
  (handler-case
    (case (intern-upcase (car command) :keyword)
      (:help (help))
      (:import-table (print-line "Table ~A imported successfully"
                                (apply #'import-table (cdr command))))
      (:select (without-style-warnings (table-print (eval command))))
      (t (print-line "Unknown command ~A" command)))
    (condition (c) (print-line "~A" c))))

(defun repl ()
  (in-package #:ptql)
  (print-line "Welcome to PTQL, enter your commands or q to quit!")
  (do ((command (read-command) (read-command)))
      ((funcall (test-safe #'string= #'symbolp)
                (car command) 'q) (print-line "Goodbye"))
      (when command (interpret-command command))))
