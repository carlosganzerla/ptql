(in-package #:ptql)

(defun help ()
  (print-line "Commands must be on the following form:")
  (print-line "import-table #p\"<csv-path>\" <table-name>")
  (print-line "select ([column]* | *) :from <table-name>")
  (print-line "[:where <condition>] [:order-by ([column | (column :desc)]*)]"))

(defun read-command ()
  (let ((*read-eval* nil)
        (command (ignore-errors 
                   (read-from-string (concatenate 'string
                                                  "("
                                                  (read-line *query-io*)
                                                  ")")))))
    (if (and command (symbolp (car command))) 
        (values (internkw (car command)) (cdr command))
        (values nil nil))))

(defun findkw-function (kw)
  (aif (find-symbol (symbol-name kw))
    (handler-case
      (values (symbol-function it) it)
      (condition () (values nil it)))
    (values nil nil)))

(defmacro command-interpreter ((cmd-key cmd-args) values-form &body forms)
  `(multiple-value-bind (,cmd-key ,cmd-args) ,values-form 
     (handler-case
       (case ,cmd-key
         ,@(mapcar (lambda (form) 
                     (let ((key (internkw (car form))))
                       `(,key
                          (let ((result (awhen (findkw-function ,key)
                                          (without-style-warnings
                                            (if ,cmd-args
                                                (apply it ,cmd-args)
                                                (funcall it))))))
                            ,@(cdr form)))))
                   forms)
         (t (print-line "Unknown command ~A" ,cmd-key)))
       (condition (c) (print-line "An error has occurred: ~A" c)))))

(defun repl ()
  (print-line "Welcome to PTQL! Enter a command or q to quit")
  (do () (nil)
      (print-line "Enter command:")
      (command-interpreter (key args) (read-command)
        (help)
        (import-table (print-line "Table ~A imported successfully" result))
        (select (table-print result))
        (q (return-from nil))))
  (print-line "Goodbye"))
