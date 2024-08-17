#! /usr/bin/env chez --script
(format #t "asds~a~%" (reverse! '(1 2 3)))

(define-record point (x y))

(define-record marble (color quality))
(define x (make-marble 'blue 'medium))
(marble? x) 
(pair? x) 
(vector? x) 
(marble-color x) 
(marble-quality x) 
(set-marble-quality! x 'low)
(marble-quality x) 

(define-record marble ((immutable color) (mutable quality))
  (((mutable shape) (if (eq? quality 'high) 'round 'unknown))))
  (marble-shape (make-marble 'blue 'high)) ;<graphic> round
  (marble-shape (make-marble 'blue 'low))  ;<graphic> unknown
  (define x (make-marble 'blue 'high))
  (set-marble-quality! x 'low)
  (marble-shape x) ;<graphic> round
  (set-marble-shape! x 'half-round)
  (marble-shape x) ;graphic> half-round
