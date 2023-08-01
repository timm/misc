(load "macros") ; test-freq.lisp

(let (count)
  (dolist (x '(aa aa aa aa bb bb cc)) 
     (incf (freq x count)))
  (print count))
