#lang racket
(provide (all-defined-out))
(require racket/struct)

;; --- Config & Global Options ---
;; Derived from ez_class.py Options
(define the (hash 'm 2 'k 1 'p 2 'Keep 256 'seed 1 'decs 2))
(define (get k) (hash-ref the k))

;; --- Counters (Sym & Num) ---
(struct sym ([counts #:mutable] [n #:mutable]) #:transparent)
(struct num ([has #:mutable] [lo #:mutable] [hi #:mutable] [ok #:mutable]) #:transparent)

(define (make-sym) (sym (make-hash) 0))
(define (make-num) (num '() 1e32 -1e32 #t))

;; --- Core Methods (add!, mid, likelihood) ---
(define (add! c v)
  (match c
    [(struct* sym ([counts counts] [n n]))
     (unless (equal? v "?")
       (hash-set! counts v (+ 1 (hash-ref counts v 0)))
       (set-sym-n! c (+ 1 n)))]
    [(struct* num ([has has] [lo lo] [hi hi]))
     (unless (equal? v "?")
       (set-num-lo! c (min v lo))
       (set-num-hi! c (max v hi))
       (set-num-has! c (cons v has))
       (set-num-ok! c #f))]) v)

(define (mid c)
  (match c
    [(struct* sym ([counts counts]))
     (first (sort (hash-keys counts) > #:key (λ (k) (hash-ref counts k))))]
    [(struct* num ([has has]))
     (let ([h (sort has <)]) 
       (if (empty? h) 0 (list-ref h (quotient (length h) 2))))]))

(define (likelihood c v)
  (match c
    [(struct* sym ([counts counts] [n n]))
     (/ (+ (hash-ref counts v 0) (* (get 'm) (/ 1 (max 1 n))))
        (+ n (get 'm)))]
    [(struct* num ([hi hi] [lo lo]))
     (let* ([m (mid c)] 
            [sd (/ (- hi lo) 6)])
       (/ (exp (- (/ (expt (- v m) 2) (* 2 (expt sd 2) 1e-32))))
          (* (+ sd 1e-32) (sqrt (* 2 pi)))))]))

;; --- Data & Column Structures ---
(struct col (at name obj))
(struct data ([rows #:mutable] [cols #:mutable]))

(define (make-cols names)
  (for/list ([n names] [i (in-naturals)])
    ;; Porting logic: Uppercase names are Nums, others are Syms
    (col i n (if (regexp-match? #px"^[A-Z]" n) (make-num) (make-sym)))))

(define (data-add! d row)
  (match-let ([(struct* data ([rows rows] [cols cols])) d])
    (if (empty? cols)
        (set-data-cols! d (make-cols row))
        (begin
          (for ([c cols])
            (match-let ([(struct* col ([at idx] [obj obj])) c])
              (add! obj (list-ref row idx))))
          (set-data-rows! d (cons row rows))))))

;; --- Bayes Inference ---
(define (loglike d row n-all nh)
  (let* ([m (get 'm)]
         [prior (/ (+ (length (data-rows d)) m) (+ n-all (* nh m)))])
    (for/fold ([out (log prior)])
              ([c (data-cols d)])
      (match-let ([(struct* col ([at idx] [obj obj])) c])
        (let ([v (list-ref row idx)])
          (if (equal? v "?") 
              out 
              (+ out (log (+ (likelihood obj v) 1e-9)))))))))

;; --- Main / eg__ Logic ---
(module+ main
  (define d (data '() '()))
  ;; Example row simulation based on ez_class.py logic
  (define header '("Name" "Age!" "Population+"))
  (data-add! d header)
  (data-add! d '("Apple" 10 100))
  (data-add! d '("Pear" 20 200))
  
  (printf "Columns: ~a\n" (map col-name (data-cols d)))
  (printf "Midpoints: ~a\n" (map (λ (c) (mid (col-obj c))) (data-cols d))))
