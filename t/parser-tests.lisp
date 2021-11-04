(in-package #:ptql/test)

(def-suite parser-tests :in ptql)
(in-suite parser-tests)

(test string-split-splits-a-string
  (is (equal (ptql::split-string "") '("")))
  (is (equal (ptql::split-string "astring") '("astring")))
  (is (equal (ptql::split-string "a s t r i n g")
             '("a" "s" "t" "r" "i" "n" "g")))
  (is (equal (ptql::split-string "  carlo   a r ")
             '("" "" "carlo" "" "" "a" "r" "")))
  (is (equal (ptql::split-string "32,carlo,alude,55," #\,)
             '("32" "carlo" "alude" "55" ""))))

(test parse-table-with-number-coercion
   (multiple-value-bind (rows cols) (ptql::parse-file #p"t/test.csv" #\;)
    (is (equal rows 
               '((:ID NIL :NAME NIL) 
                 (:ID 1 :NAME NIL)
                 (:ID NIL :NAME NIL :AGE NIL :HEIGHT NIL :WEIGHT NIL)
                 (:ID 3 :NAME NIL :AGE 3 :HEIGHT NIL)
                 (:ID 6 :NAME "joe" :AGE 33 :HEIGHT 1.86 :WEIGHT "93")
                 (:ID 6 :NAME "mark" :AGE 20 :HEIGHT 1.55 :WEIGHT "NO DATA")
                 (:ID 7 :NAME "bill" :AGE 15 :HEIGHT 2.11 :WEIGHT "129")
                 (:ID 8 :NAME "midas" :AGE 45 :HEIGHT 1.7 :WEIGHT "65")
                 (:ID NIL) (:ID NIL))))
    (is (equal cols '(:ID :NAME :AGE :HEIGHT :WEIGHT)))))

(test parse-table-without-number-coercion
   (multiple-value-bind (rows cols) (ptql::parse-file #p"t/test.csv" #\; nil)
    (is (equal rows 
               '((:ID NIL :NAME NIL)
                 (:ID "1" :NAME NIL)
                 (:ID NIL :NAME NIL :AGE NIL :HEIGHT NIL :WEIGHT NIL)
                 (:ID "3" :NAME NIL :AGE "03" :HEIGHT NIL)
                 (:ID "6" :NAME "joe" :AGE "33" :HEIGHT "1.86" :WEIGHT "93")
                 (:ID "6" :NAME "mark" :AGE "20" :HEIGHT "0001.55"
                  :WEIGHT "NO DATA")
                 (:ID "7" :NAME "bill" :AGE "15" :HEIGHT "2.11" :WEIGHT "129")
                 (:ID "8" :NAME "midas" :AGE "45" :HEIGHT "1.70" :WEIGHT "65")
                 (:ID NIL)
                 (:ID NIL))))
    (is (equal cols '(:ID :NAME :AGE :HEIGHT :WEIGHT)))))
