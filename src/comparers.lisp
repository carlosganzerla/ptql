(write-to-string (list 1 2 3 4))

(defun lambda-clean (lambda-list)
  (remove-if (lambda (sym)
               (char= (char (symbol-name sym) 0) #\&))
             lambda-list))

(defmacro defcomparer (name)
  (let* ((num-op name)
         (string-op (intern (concat "STRING" (symbol-name num-op))))
         (op (intern (concat "." (symbol-name num-op)))))
    `(defun ,op (fst snd)
       (funcall
         (cond ((or (not fst) (not snd)) (lambda (&rest args) nil))
               ((and (stringp fst) (stringp snd)) #',string-op)
               ((and (numberp fst) (numberp snd)) #',num-op)
               (t (lambda (x y)
                    (,string-op (or (stringp x) (write-to-string x))
                                (or (stringp y) (write-to-string y))))))
         fst snd))))

(defcomparer =)

(defcomparer .>= string>= (fst snd))

(defcomparer .<= string<= (fst snd))

(defcomparer .> string> (fst snd))

(defcomparer .< string< (fst snd))

(defcomparer ./= string/= (fst snd))

(defcomparer .+ string/= (fst snd))

(defcomparer .- string/= (fst snd))

(defcomparer .* string/= (fst snd))
