
(import (rnrs))

(define gcd
  (lambda (m n)
    (if (and (>= m 0) (>= n 0))
      (cond ((= n 0) m)
            (#t (gcd n (mod m n))))
      (- 1))))

(display (gcd 119 544))

(define-record-type point (fields (mutable x) y))

(let ((p (make-point 1 2)))
  (point-x-set! p (- (point-x p) 12))
  (display p))

