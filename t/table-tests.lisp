(in-package #:ptql/test)

(def-suite table-tests :in ptql)
(in-suite table-tests)

(defvar *table* 
  (make-instance 'table 
                 :rows 
                 '((:ID NIL :NAME NIL) 
                   (:ID 1 :NAME NIL)
                   (:ID NIL :NAME NIL :AGE NIL :HEIGHT NIL :WEIGHT NIL)
                   (:ID 3 :NAME NIL :AGE 3 :HEIGHT NIL)
                   (:ID 6 :NAME "joe" :AGE 33 :HEIGHT 1.86 :WEIGHT "93")
                   (:ID 6 :NAME "mark" :AGE 20 :HEIGHT 1.55 :WEIGHT "NO DATA")
                   (:ID 7 :NAME "bill" :AGE 15 :HEIGHT 2.11 :WEIGHT "129")
                   (:ID 8 :NAME "midas" :AGE 45 :HEIGHT 1.7 :WEIGHT "65")
                   (:ID NIL) (:ID NIL))
                 :columns '(:ID :NAME :AGE :HEIGHT :WEIGHT)))
                

(test get-cell-gets-a-cell-for-a-row
  (is (= 3 (get-cell *table*  :id (nth 3 (rows *table*)))))
  (is (not (get-cell *table*  :height (car (rows *table*)))))
  (is (not (get-cell *table*  :name (cadr (rows *table*)))))
  (is (= 20 (get-cell *table*  :age (nth 5 (rows *table*)))))
  (is (string= "65" (get-cell *table* :weight (nth 7 (rows *table*))))))


(test get-cell-on-unknown-column-fails
  (signals condition (get-cell *table* :not-here (nth 3 (rows *table*))))
  (signals condition (get-cell *table* :bulba 'x))
  (signals condition (get-cell *table* :yyy "IM NOT A ROW DUDE")))
  
