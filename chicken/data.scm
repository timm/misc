(use srfi-1 srfi-13 srfi-69)

;; ------------------------------------------------------------
;; helpers
;; ------------------------------------------------------------
(define (uc1? s)
  (and (> (string-length s) 0)
       (char-upper-case? (string-ref s 0))))

(define (lastc s)
  (string-ref s (- (string-length s) 1)))

;; ------------------------------------------------------------
;; Sym
;; ------------------------------------------------------------
(define-record-type Sym
  (make-Sym n at txt has) Sym?
  (n   Sym-n  Sym-n!)
  (at  Sym-at)
  (txt Sym-txt)
  (has Sym-has))

(define (Sym-new at txt)
  (make-Sym 0 at txt (make-hash-table)))

;; ------------------------------------------------------------
;; Num
;; ------------------------------------------------------------
(define-record-type Num
  (make-Num n at txt mu m2 sd lo hi more) Num?
  (n    Num-n   Num-n!)
  (at   Num-at)
  (txt  Num-txt)
  (mu   Num-mu  Num-mu!)
  (m2   Num-m2  Num-m2!)
  (sd   Num-sd  Num-sd!)
  (lo   Num-lo  Num-lo!)
  (hi   Num-hi  Num-hi!)
  (more Num-more))

(define (Num-new at txt)
  (make-Num 0 at txt 0.0 0.0 0.0 1e32 -1e32
            (if (char=? (lastc txt) #\-) 0 1)))

;; ------------------------------------------------------------
;; Cols
;; ------------------------------------------------------------
(define-record-type Cols
  (make-Cols names all x y klass) Cols?
  (names Cols-names)
  (all   Cols-all)
  (x     Cols-x)
  (y     Cols-y)
  (klass Cols-klass))

(define (Cols-new names)
  (define (make-col i s)
    ((if (uc1? s) Num-new Sym-new) i s))
  (define (tag s)
    (case (lastc s)
      ((#\X) 'skip)
      ((#\!) 'klass)
      ((#\-) 'y)
      ((#\+) 'y)
      (else  'x)))
  (let loop ((i 0) (ns names) (all '()) (x '()) (y '()) (klass #f))
    (if (null? ns)
        (make-Cols names
                   (reverse all)
                   (reverse x)
                   (reverse y)
                   klass)
        (let* ((s   (car ns))
               (col (make-col i s))
               (t   (tag s)))
          (loop (+ i 1) (cdr ns)
                (cons col all)
                (if (eq? t 'x) (cons col x) x)
                (if (eq? t 'y) (cons col y) y)
                (if (eq? t 'klass) col klass))))))

;; ------------------------------------------------------------
;; Data
;; ------------------------------------------------------------
(define-record-type Data
  (make-Data n mid rows cols) Data?
  (n    Data-n    Data-n!)
  (mid  Data-mid  Data-mid!)
  (rows Data-rows Data-rows!)
  (cols Data-cols))

(define (data-add-row! d row)
  (Data-rows! d (cons row (Data-rows d)))
  (Data-n! d (+ 1 (Data-n d)))
  d)

(define (adds rows d)
  (for-each (lambda (r) (data-add-row! d r)) rows)
  d)

;; src: list of rows; first row is header (names)
(define (Data-new src)
  (let* ((names (car src))
         (rows0 (cdr src))
         (cols  (Cols-new names))
         (d     (make-Data 0 #f '() cols)))
    (adds rows0 d)))

;; ------------------------------------------------------------
;; clone
;; ------------------------------------------------------------
(define (clone data #!optional rows)
  (Data-new (cons (Cols-names (Data-cols data)) (or rows '()))))
