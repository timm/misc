(load "macros") ; test-with-csv.lisp

(let ((n 0))
  (print 
    (with-csv (line "auto93.csv" n)
      (incf n (length line)))))
