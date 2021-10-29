(write-to-string (list 1 2 3 4))

(defun lambda-clean (lambda-list)
  (remove-if (lambda (sym)
               (char= (aref (symbol-name sym) 0) #\&))
             lambda-list))

(defmacro defcomparer (name string-fn params)
  `(defun ,name ,params
     (apply #',string-fn 
            (mapcar (lambda (x)
                      (if (stringp x) x (write-to-string x))) 
                    (mklist ,@(lambda-clean params))))))

(defcomparer .= string= (fst snd))

(defcomparer .>= string>= (fst snd))

(defcomparer .<= string<= (fst snd))

(defcomparer .> string> (fst snd))

(defcomparer .< string< (fst snd))

(defcomparer ./= string/= (fst snd))

(defcomparer .+ string/= (fst snd))

(defcomparer .- string/= (fst snd))

(defcomparer .* string/= (fst snd))
