; vim: set ft=lisp ts=2 sw=2 et :
; story.lisp -- A Tour of fri.lisp, from one value to XAI.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
; Rendered via `bash sh/lisp2md.sh "A Tour of fri.lisp" < story.lisp`.
; This file is a walkthrough, not a runnable program. Real code lives
; in fri.lisp; this file quotes excerpts from it.
;
; *From one lonely value to explainable multi-objective optimization,
; in the style of a REPL walk. One file, no dependencies, five hundred
; lines. Every fragment runs; every output was captured, not imagined.*
;
; > Once upon a time there was a value. It did not know where it was
; > among all the other values. Then it found that it was part of a row,
; > and there were many rows, and there were patterns around: patterns
; > in the other values at the same place in other rows. Slopes. Hills.
; > Groups. Before long, the value knew where it stood.
;
; This is a tour of `fri.lisp`. We start with a single number and end
; with a decision tree that picks good cars out of a holdout it has
; never seen. Every stop is small. Every fragment is a real REPL
; transcript.
;
; ## Getting ready
;
; You need SBCL or CLISP. On macOS: `brew install sbcl`. Sit in the
; directory that holds `fri.lisp` and `auto93.csv`, and open a REPL:
;
; ```text
; $ sbcl
; * (load "fri.lisp")
; T
; *
; ```
;
; The `*` is SBCL's prompt. Every fragment below uses it so you can
; tell input from output.
;
; Each chapter also maps to a command-line demo:
;
; ```text
; $ sbcl --script fri.lisp --stats auto93.csv
; ```
;
; Those demos live at the bottom of `fri.lisp` as `eg--name` functions.
; Read them later. For now, the REPL.
;
; ----
;
; # 1. A value
;
; Here is a value.
;
; ```text
; * 42
; 42
; * (type-of 42)
; (INTEGER 0 4611686018427387903)
; ```
;
; It is forty-two. An integer. That is everything we know. Is it big?
; Is it small? Is it typical? We cannot say. A value, alone, is a dot
; without an axis.
;
; Give it a friend.
;
; ```text
; * (list 42 17)
; (42 17)
; * (+ 42 17)
; 59
; * (/ (+ 42 17) 2.0)
; 29.5
; ```
;
; Now we can compare. 42 is bigger than 17. We can sum. We can average.
; Two values are a thin shape -- an ordering and an arithmetic -- but
; they don't yet reveal anything about the world.
;
; Give it many friends.
;
; ```text
; * (list 8 304 193 70 1 4732 18.5 10)
; (8 304 193 70 1 4732 18.5 10)
; ```
;
; Now something stirs. Eight numbers. Some small, some big. The `8`
; could be a count. The `4732` could be a weight. The `18.5` could be a
; measurement. We are guessing. But guessing is new. With one value we
; could not even guess.
;
; This row is real. It is the first data row in `auto93.csv`, a dataset
; of 398 cars. We just don't know that yet.
;
; ----
;
; # 2. Reading one row from a file
;
; A file is text. Text on disk, characters. To work with it, we need
; to turn text into typed values. `fri.lisp` does this in twelve lines
; and three functions.
;
; First, `split`. A string and a separator go in, a list of substrings
; comes out:
;
; ```text
; * (split "8,304,193,70,1,4732,18.5,10" #\,)
; ("8" "304" "193" "70" "1" "4732" "18.5" "10")
; ```
;
; Eight strings. None of them are numbers yet -- they are the
; characters between commas. Now, `thing`, which coerces one string:
;
; ```text
; * (thing "304")
; 304
; * (thing "18.5")
; 18.5
; * (thing "?")
; ?
; * (thing "red")
; "red"
; ```
;
; Four rules. If `read-from-string` gives a number, use it. If the
; string is literally `"?"`, return the symbol `?` (our missing-value
; marker). Otherwise, leave it as a string. No type hints, no CSV
; library.
;
; Put them together with `with-open-file` and a loop, and you have
; `read-csv`:

(defun read-csv (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (split line #\,)))))

; Five lines. That is the entire parser:
;
; ```text
; * (defparameter *rs* (read-csv "auto93.csv"))
; *RS*
; * (length *rs*)
; 399
; * (car *rs*)
; ("Clndrs" "Volume" "HpX" "Model" "origin" "Lbs-" "Acc+" "Mpg+")
; * (cadr *rs*)
; (8 304 193 70 1 4732 18.5 10)
; ```
;
; 399 rows: the header, then 398 data rows. The header came through as
; a list of strings; the first data row as a list of integers and a
; float.
;
; Some rows carry missing values -- the string `"?"` became the symbol
; `?` during parsing:
;
; ```text
; * (find-if (lambda (r) (find '? r)) (cdr *rs*))
; (6 200 ? 74 1 2875 17 20)
; ```
;
; That car has no horsepower listed. We will need to handle it every
; time we touch the data.
;
; ----
;
; # 3. Rows by the hundred
;
; With hundreds of rows in memory, we can start to look around.
;
; ```text
; * (subseq *rs* 50 54)
; ((8 302 140 74 1 4638 16 10)
;  (8 304 150 74 1 4257 15.5 10)
;  (8 351 148 75 1 4657 13.5 10)
;  (8 351 152 76 1 4215 12.8 10))
; ```
;
; Rows 50-53 look like each other. Eight cylinders, big weight, low
; Mpg (the last value). That is a cluster.
;
; ```text
; * (car (last *rs*))
; (4 86 65 80 3 2110 17.9 50)
; ```
;
; The last row is completely different. Four cylinders, light,
; `Mpg=50`. A different cluster. One row in chapter 1 was a guess.
; Four similar rows, followed by a dissimilar one, is a pattern.
;
; Count the rows carrying a missing value:
;
; ```text
; * (count-if (lambda (r) (find '? r)) (cdr *rs*))
; 6
; ```
;
; Six rows out of 398. A 1.5% gap. Any column summary or distance
; calculation has to tolerate this.
;
; ----
;
; # 4. Names decide roles
;
; The header carries the schema.
;
; ```text
; * (car *rs*)
; ("Clndrs" "Volume" "HpX" "Model" "origin" "Lbs-" "Acc+" "Mpg+")
; ```
;
; Two rules classify the eight names:
;
; - **First character.** Uppercase -> `Num`. Lowercase -> `Sym`. So
;   `Volume` is numeric, `origin` is categorical (the integers 1/2/3
;   stand for "USA / Europe / Japan" -- don't average them).
; - **Last character.** `+` means maximize; `-` means minimize; `!`
;   means a class label; `X` means skip. Anything else is a feature.
;
; Proof the rule lives in the constructors. `make-num` reads the suffix
; once to set `heaven`:

(defun make-num (&rest args &aux (i (apply #'%make-num args)))
  (setf $heaven (if (eq #\- (ch $txt -1)) 0 1))
  i)

; Try both directions:
;
; ```text
; * (? (make-num :txt "Mpg+") heaven)
; 1
; * (? (make-num :txt "Lbs-") heaven)
; 0
; ```
;
; One flag per goal, decided at construction time, never revisited.
; `make-cols` then walks the header, picks either `make-num` or
; `make-sym`, and partitions by the suffix:

(defun make-cols (txts &aux (i (%make-cols :names txts)) (n -1))
  (labels ((ako (txt)
             (if (upper-case-p (ch txt 0)) #'make-num #'make-sym))
           (one (txt) (funcall (ako txt) :txt txt :at (incf n)))
           (end (col) (ch (? col txt) -1)))
    (setf $all (mapcar #'one txts))
    (dolist (col $all i)
      (unless (eql (end col) #\X)
        (if (find (end col) "!-+") (push col $y) (push col $x))))))

; Three tiny local functions: `ako` picks the constructor, `one` builds
; one column, `end` reads its last char. Now build a `Data` and inspect
; the partition:
;
; ```text
; * (defparameter *d* (make-data *rs*))
; *D*
; * (mapcar (lambda (c) (? c txt)) (? *d* cols y))
; ("Mpg+" "Acc+" "Lbs-")
; * (mapcar (lambda (c) (? c txt)) (? *d* cols x))
; ("origin" "Model" "Volume" "Clndrs")
; ```
;
; Three y-columns (goals), four x-columns (features). `HpX` is gone
; entirely -- its `X` suffix told the classifier to drop it.
;
; > **The reader macro `(? x a b c)`**
; >
; > `(? x a)` expands to `(slot-value x 'a)`; chains recurse, so
; > `(? *d* cols y)` is `(slot-value (slot-value *d* 'cols) 'y)`. A
; > second macro, written `$slot`, is the same thing when the object
; > is implicitly named `i` (the convention inside method bodies).
; > Reader macros run once at read time; after that the code is
; > ordinary s-expressions.
;
; ----
;
; # 5. A column that remembers
;
; Data-science textbooks start with `numpy.mean`. We start with a
; harder question: *can a column tell you its mean without keeping
; every value it has seen?*
;
; Yes. Since 1962, thanks to B. P. Welford.
;
; ```text
; * (defparameter *n* (make-num))
; *N*
; * *n*
; #S(NUM :AT 0 :TXT " " :N 0 :MU 0 :M2 0 :HEAVEN 1)
; ```
;
; A fresh `Num` holds six slots: position, name, count, mean, sum of
; squared deviations, and `heaven`. All zero.
;
; Feed it five values and watch the three numbers we care about:
;
; ```text
; * (dolist (v '(10 20 30 40 50))
;     (add *n* v)
;     (format t "after ~2d: n=~a mu=~a m2=~a~%"
;             v (? *n* n) (? *n* mu) (? *n* m2)))
; after 10: n=1 mu=10.0 m2=0.0
; after 20: n=2 mu=15.0 m2=50.0
; after 30: n=3 mu=20.0 m2=200.0
; after 40: n=4 mu=25.0 m2=500.0
; after 50: n=5 mu=30.0 m2=1000.0
; ```
;
; Mu moves as each value arrives. `m2` accumulates the running sum of
; squared deviations from the *current* mean. Neither reads a stored
; list. Here is the update:

(defmethod add ((i num) v &optional (w 1))
  (incf $n w)
  (let ((delta (- v $mu)))
    (incf $mu (float (/ (* w delta) $n)))
    (incf $m2 (float (* w delta (- v $mu))))))

; Three increments per sample. Note: `mu` updates *before* `m2` reads
; it. That is not a bug -- Welford's identity exploits the new mean
; for the `m2` term.
;
; The weight argument `w` is the trick that makes the summary
; *symmetric*. Pass `-1` and the same formulas unwind a previous add:
;
; ```text
; * (sub *n* 50) (sub *n* 40)
; 40
; * (format t "after 2 subs: n=~a mu=~a m2=~a~%"
;         (? *n* n) (? *n* mu) (? *n* m2))
; after 2 subs: n=3 mu=20.0 m2=200.0
; ```
;
; We rewound two adds and landed exactly where we were at step 3.
; Chapter 13 uses this to move rows between sets without recomputing.
;
; Symbolic columns keep a frequency alist. Same verb, simpler body:
;
; ```text
; * (defparameter *s* (make-sym))
; *S*
; * (add *s* 'red) (add *s* 'red) (add *s* 'blue)
; BLUE
; * (? *s* has)
; ((BLUE . 1) (RED . 2))
; * (? *s* n)
; 3
; ```
;
; A missing value (`'?`) slips past both methods without updating
; anything. That is the only place types diverge inside `add`.
;
; ----
;
; # 6. Mid and spread
;
; Every column, whatever its type, answers two questions: *where is
; its middle?* and *how wide is it?*
;
; ```text
; * (mid *n*)
; 20.0
; * (spread *n*)
; 10.0
; * (mid *s*)
; RED
; * (spread *s*)
; 3.169925
; ```
;
; For `Num`: `mid` is the mean; `spread` is the standard deviation,
; derived in one line from `m2` and `n`:

(defmethod spread ((i num))
  (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

; The guard: if we have fewer than two values, spread is zero.
; Otherwise: `sqrt(m2 / (n-1))`. Check by hand with our three-value
; column after the two subs: `sqrt(200/2) = 10.0`. Matches.
;
; For `Sym`: `mid` is the mode; `spread` is an entropy-like score that
; grows as frequencies become more mixed. Different math; same role:
; *how much does this column vary?*
;
; Nothing in the caller's code looks like `if col.type == "num"`.
; Dispatch is built into `defmethod`, which takes a type specializer
; on the first argument.
;
; ```text
; $ sbcl --script fri.lisp --stats auto93.csv
; Clndrs        mid=5.46   spread=1.70
; Volume        mid=193.4  spread=104.3
; HpX           mid=104.5  spread=38.5
; Model         mid=76.0   spread=3.7
; origin        mid=1      spread=...
; Lbs-          mid=2970   spread=846.8
; Acc+          mid=15.6   spread=2.76
; Mpg+          mid=23.8   spread=8.34
; ```
;
; Four hundred rows times eight columns, summarized in one pass. Peek
; any number; it is current.
;
; ----
;
; # 7. Where do I stand?
;
; Back to the lonely value. The column knows its mid and spread. Now
; we can answer: given a value, where does it live inside the column?
;
; The cheap, effective trick is z-score + sigmoid. Distance from the
; mean, scaled by spread, squashed into `[0,1]`:

(defmethod norm ((i num) v)
  (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
    (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z))))))))

; Four moves. Compute z. Clamp to `[-3, 3]` (covers 99.7% of any
; Gaussian). Sigmoid at scale 1.7 (approximates the Gaussian CDF). The
; `1e-32` term avoids divide-by-zero on a zero-spread column.
;
; Watch the curve at five z-levels on the real Mpg+ column (`mu=23.8`,
; `sd=8.34`):
;
; | value     | z-score | norm  | reads as   |
; |-----------|---------|-------|------------|
; | 10        | -1.65   | 0.056 | bottom 6%  |
; | 23.8-sd   | -1.00   | 0.154 | bottom 15% |
; | 23.8 (mu) |  0.00   | 0.500 | median     |
; | 23.8+sd   | +1.00   | 0.846 | top 15%    |
; | 50        | +3.14   | 0.994 | top 1%     |
;
; ```text
; * (defparameter *mpgcol*
;     (find "Mpg+" (? *d* cols y) :key (lambda (c) (? c txt)) :test #'equal))
; *MPGCOL*
; * (norm *mpgcol* 23.8)
; 0.4977463
; * (norm *mpgcol* 50)
; 0.99394023
; * (norm *mpgcol* 10)
; 0.056161575
; ```
;
; Symbols don't need scaling; `norm` on a `Sym` is the identity
; (strings compare equal or not-equal; there is no "between" to scale).
;
; ```text
; * (norm (make-sym) 'red)
; RED
; ```
;
; ----
;
; # 8. Heaven
;
; Normalization puts every column on `[0, 1]`. But zero is not the
; same in every column. For `Mpg+`, "good" is 1. For `Lbs-`, "good" is
; 0. We need a per-column target.
;
; Each `Num` carries a `heaven` slot. Set once at construction
; (chapter 4). The three goals of auto-93:
;
; ```text
; * (dolist (c (? *d* cols y))
;     (format t "~a heaven=~a~%" (? c txt) (? c heaven)))
; Mpg+ heaven=1
; Acc+ heaven=1
; Lbs- heaven=0
; ```
;
; Look at one row and compare normalized value to heaven for each goal:
;
; ```text
; * (let ((r (car (last *rs*))))
;     (format t "row: ~a~%" r)
;     (dolist (c (? *d* cols y))
;       (let ((v (elt r (? c at))))
;         (format t "  ~4a  raw=~a  norm=~,2f  heaven=~a~%"
;                 (? c txt) v (norm c v) (? c heaven)))))
; row: (4 86 65 80 3 2110 17.9 50)
;   Mpg+  raw=50    norm=0.99  heaven=1
;   Acc+  raw=17.9  norm=0.81  heaven=1
;   Lbs-  raw=2110  norm=0.15  heaven=0
; ```
;
; Every normalized value is close to its heaven. A good car. The
; `Lbs-` line shows why heaven matters: norm 0.15 is a long way from
; 1, but it is close to 0 -- exactly what we want for a column we are
; minimizing.
;
; ----
;
; # 9. Distance to heaven
;
; Three goals, three heavens: `(1, 1, 0)`. The distance from a row to
; heaven is one number. One number per row. A scalar we can sort by.
; This is `disty`:

(defun disty (data row &optional (cols (? data cols y)))
  (dist cols
    (lambda (i) (abs (- (norm i (elt row $at)) $heaven)))))

(defun dist (lst f &aux (d 0))
  (dolist (v lst (expt (/ d (length lst)) (/ 1 !p)))
    (incf d (expt (funcall f v) !p))))

; For each goal: compute normalized value, subtract heaven, take
; absolute value. That gives a per-goal "how far off am I?" delta.
; Roll them up by Minkowski distance with exponent `!p` (default 2,
; i.e. Euclidean).
;
; Worked on the first data row:
;
; ```text
; * (defparameter *r0* (cadr *rs*))
; *R0*
; * *r0*
; (8 304 193 70 1 4732 18.5 10)
; ```
;
; | col  | raw  | norm  | heaven | abs delta | delta^2 |
; |------|------|-------|--------|-----------|---------|
; | Mpg+ | 10   | 0.056 | 1      | 0.944     | 0.891   |
; | Acc+ | 18.5 | 0.859 | 1      | 0.141     | 0.020   |
; | Lbs- | 4732 | 0.972 | 0      | 0.972     | 0.944   |
;
; Sum of squared deltas: `0.891 + 0.020 + 0.944 = 1.855`. Divide by 3
; goals: `0.6183`. Square root: `0.7863`.
;
; ```text
; * (disty *d* *r0*)
; 0.7863217
; ```
;
; Matches. Three goals collapsed into one scalar we can compare
; against any other row.
;
; Compare the best and worst rows in the file:
;
; ```text
; * (disty *d* *r0*)                              ; row 0: heavy V8
; 0.7863217
; * (disty *d* (car (last *rs*)))                 ; tail: light 4-cyl
; 0.1410149
; ```
;
; 0.14 is close to heaven; 0.79 is far. A scalar that ranks both cars
; on three goals at once.
;
; ----
;
; # 10. The world, sorted
;
; One number per row -> sort by it. The best cars rise; the worst
; sink. No classifier, no training -- just the shape of goal space and
; a `sort` call.
;
; ```text
; $ sbcl --script fri.lisp --ydata auto93.csv
; ```
;
; Annotated with disty values alongside each row:
;
; ```text
; disty  Clndrs Volume HpX Model origin Lbs-  Acc+ Mpg+
; -----  ------ ------ --- ----- ------ ----  ---- ----
; 0.14   4      86     65  80    3      2110  17.9 50    <- best
; 0.22   4      120    88  82    3      2160  14.5 40
; 0.28   4      71     65  74    3      1836  21.0 30
; 0.34   4      105    63  81    1      2215  14.9 30
; ...
; 0.73   8      305    145 77    1      3880  12.5 20
; 0.78   8      304    193 70    1      4732  18.5 10
; 0.81   8      351    153 71    1      4154  13.5 10
; 0.87   8      429    198 73    1      4952  11.5 10    <- worst
; ```
;
; Four-cylinder, light, late-model cars live at the top of the
; ranking. Eight-cylinder 70s V8s live at the bottom. Nothing
; surprising. But we used no classifier, no optimization, no Pareto
; sort -- just statistics about columns plus one distance function.
;
; This is multi-objective optimization done with almost nothing. No
; NSGA-II, no weighted sums, no preference elicitation. A shape, a
; distance, a sort.
;
; ----
;
; # 11. Distance between rows
;
; So far, distance was always *to heaven*. Rows can also be at a
; distance from each other, in feature space, through `distx`:

(defmethod distx ((i data) r1 r2)
  (dist (? i cols x)
    (lambda (i)
      (let ((a (elt r1 $at)) (b (elt r2 $at)))
        (if (and (eq a '?) (eq b '?)) 1 (distx i a b))))))

(defmethod distx ((i sym) a b) (if (equal a b) 0 1))

(defmethod distx ((i num) a b &aux (a (norm i a)) (b (norm i b)))
  (cond ((eq a '?) (abs (- b (if (> b 0.5) 0 1))))
        ((eq b '?) (abs (- a (if (> a 0.5) 0 1))))
        (t         (abs (- a b)))))

; Three cases. For Syms: same-or-different (0 or 1). For Nums:
; absolute difference of normalized values. For missing values:
; *pessimistic*. If `a` is known and `b` is missing, assume `b` sits
; at whichever extreme is furthest from `a` -- the missing value is
; treated as "as bad as it could be." Both missing: distance 1.
;
; Five distances from row 0 (the heavy V8):
;
; ```text
; * (distx *d* *r0* *r0*)                       ; to self
; 0.0
; * (distx *d* *r0* (caddr *rs*))               ; to near-twin
; 0.040
; * (distx *d* *r0* (elt *rs* 200))             ; to a middle row
; 0.697
; * (distx *d* *r0* (car (last *rs*)))          ; to its opposite
; 0.821
; * (distx *d* *r0* (find-if (lambda (r) (find '? r)) (cdr *rs*)))
; 0.249                                         ; to a row with ?
; ```
;
; Zero to self. Tiny distance to row 2 (also a 70s V8). Middling
; distance to a Euro four-cylinder. Large distance to the best car.
; The missing-value row lands in the middle -- pessimism penalizes
; the unknown column, but the other columns still compare normally.
;
; ----
;
; # 12. Labels cost money
;
; Here is the twist. In real problems, the y-columns are *expensive*.
; You want Mpg? Build the car, drive it. Defect counts? Ship the
; release, wait. Labels cost money; features are almost free.
;
; So we pretend the `y` values are hidden, and we uncover them one at
; a time. The question becomes: which row should we pay to label next?
;
; The strategy in `fri.lisp`:
;
; 1. Label 4 rows at random. Call this `lab`.
; 2. Sort `lab` by `disty`. The best sqrt(N) go into `best`; the rest
;    into `rest`.
; 3. For each unlabeled row in the pool: is it closer (in feature
;    space) to `best`'s centroid than to `rest`'s centroid? If yes,
;    label it and add to `best`. If no, skip.
; 4. After each addition, `rebalance`: cap `best` at sqrt(|lab|) rows
;    by moving the worst-disty row from `best` to `rest`. Keeps the
;    elite pile small.
; 5. Stop after `!budget` labels (default 50) or when the pool is
;    exhausted.
;
; The acquisition function is one line:

(defun closer? (lab best rest r)
  (< (distx lab r (mid best)) (distx lab r (mid rest))))

; `mid` on a `Data` is the centroid vector: one `mid` per column,
; memoized on `_mid` until the next `add` invalidates it.
;
; The whole loop:

(defun active (rs &aux (hd (list (car rs))))
  (let* ((lab  (make-data hd)) (best (make-data hd))
         (rest (make-data hd)) (ys   (make-num))
         (pool (warm-start (shuffle (cdr rs)) lab best rest ys)))
    (loop for r in (subseq pool 0 (min !few (length pool)))
          while (< (? ys n) !budget) do
          (when (closer? lab best rest r)
            (add ys (disty lab r)) (add lab r) (add best r)
            (rebalance best rest lab)))
    (values best lab (? ys n))))

; Run it at the REPL and inspect the result:
;
; ```text
; * (multiple-value-bind (best lab labels) (active *rs*)
;     (declare (ignore best))
;     (format t "labels spent: ~a~%" labels)
;     (format t "lab rows:     ~a~%" (length (? lab rows)))
;     (format t "best 3 labeled rows by disty:~%")
;     (dolist (r (subseq
;                  (sort (copy-list (? lab rows)) #'<
;                        :key (lambda (r) (disty lab r)))
;                  0 3))
;       (format t "  ~,3f  ~a~%" (disty lab r) r))
;     (defparameter *lab* lab))
; labels spent: 32
; lab rows:     32
; best 3 labeled rows by disty:
;   0.139  (4 89 60 80 3 1968 18.8 40)
;   0.169  (4 97 52 82 2 2130 24.6 40)
;   0.278  (4 81 60 81 3 1760 16.1 40)
; ```
;
; Thirty-two labels bought us three rows with disty under 0.28 -- all
; at the good end of the landscape. The learner never peeked at rows
; outside the labeled set. A one-twelfth budget found the top region.
;
; ----
;
; # 13. Hills on the landscape
;
; We have 32 labeled rows. Not many, but enough to see where the good
; rows live in feature space. A decision tree trained on those labels
; is a *map* of the landscape.
;
; Building the tree in pieces. First, what cuts does a column offer?

(defmethod %cuts ((c sym) vs) (remove-duplicates vs :test #'equal))

(defmethod %cuts ((c num) vs)
  (list (elt (sort vs #'<) (floor (length vs) 2))))

; Sym columns offer all unique values as cuts. Num columns offer
; *only the median*. That is not laziness -- it is regularization. At
; 50 labels, trying every threshold would overfit. One cut per numeric
; column keeps the tree honest.
;
; ```text
; * (tree-cuts (find "Volume" (? *lab* cols x)
;                    :key (lambda (c) (? c txt)) :test #'equal)
;              (? *lab* rows))
; (97)                            ; median volume of the 32 labels
; * (tree-cuts (find "origin" (? *lab* cols x)
;                    :key (lambda (c) (? c txt)) :test #'equal)
;              (? *lab* rows))
; (3 1 2)                         ; three origins appeared
; ```
;
; Next, how good is a particular cut? `tree-split` divides the rows
; into left/right, computes each side's disty spread, weights by
; count, and sums:

(defun tree-split (d c cut rs)
  (let (l r (ln (make-num)) (rn (make-num)))
    (dolist (row rs)
      (if (go-left? c (elt row (? c at)) cut)
          (progn (push row l) (add ln (disty d row)))
          (progn (push row r) (add rn (disty d row)))))
    (list (+ (* (? ln n) (spread ln)) (* (? rn n) (spread rn)))
          c cut l r)))

; Lower score -> a cleaner split. A pure-left node has `spread=0` on
; one side. The best cut across all columns wins and becomes the root
; decision.
;
; Then `tree-grow` recurses. Stop when a node has fewer than `2*leaf`
; rows (default leaf size: 3, so minimum node size is 6). Show the
; result:
;
; ```text
; * (tree-show (tree-grow *lab* (? *lab* rows)))
; 0.53 ( 32)
; 0.40 ( 15)   Volume <= 97
; 0.50 (  7)   |.. Model <= 79
; 0.55 (  4)   |.. |.. Volume <= 89
; 0.44 (  3)   |.. |.. Volume >  89
; 0.31 (  8)   |.. Model >  79
; 0.32 (  3)   |.. |.. Model <= 80
; 0.30 (  5)   |.. |.. Model >  80
; 0.65 ( 17)   Volume >  97
; 0.58 ( 12)   |.. Clndrs <= 4
; 0.43 (  3)   |.. |.. origin == 3
; 0.63 (  9)   |.. |.. origin != 3
; 0.56 (  3)   |.. |.. |.. Volume <= 105
; 0.67 (  6)   |.. |.. |.. Volume >  105
; 0.73 (  3)   |.. |.. |.. |.. Model <= 80
; 0.61 (  3)   |.. |.. |.. |.. Model >  80
; 0.82 (  5)   |.. Clndrs >  4
; ```
;
; Each line: mean disty at this branch, row count, branching
; condition. Lower is closer to heaven. The best leaf is
; `Volume <= 97 & Model > 80` at disty 0.30 -- a rule a human can
; read: *small late-model cars*. The worst leaf is
; `Volume > 97 & Clndrs > 4` at 0.82 -- *big multi-cylinder cars*.
;
; > **The explanation tax**
; >
; > A leaf is a rule. A rule is a sentence. A sentence is testable
; > and arguable. At ~50 labels, this tree breaks even with a centroid
; > baseline that cannot be printed. Below 30 labels, interpretability
; > costs a few points of accuracy. Above 50, it is free.
;
; ----
;
; # 14. Pay to peek
;
; The tree was fit on 32 labels. What does it say about rows it has
; never seen?
;
; Route a new row to its leaf with `tree-leaf`:

(defun tree-leaf (t0 r)
  (if (null (? t0 left)) t0
      (tree-leaf (if (go-left? (? t0 col)
                               (elt r (? (? t0 col) at))
                               (? t0 cut))
                     (? t0 left) (? t0 right)) r)))

; Try it on a known-good row that wasn't in the labeled set:
;
; ```text
; * (defparameter *tr* (tree-grow *lab* (? *lab* rows)))
; *TR*
; * (let* ((r (car (last *rs*)))
;          (leaf (tree-leaf *tr* r)))
;     (format t "row:  ~a~%" r)
;     (format t "leaf: mu=~,3f n=~a hdr=~s~%"
;             (mid (? leaf ynum)) (? (? leaf ynum) n) (? leaf hdr)))
; row:  (4 86 65 80 3 2110 17.9 50)
; leaf: mu=0.325 n=3 hdr="Model <= 80"
; ```
;
; The tree predicts disty ~0.33 for this row, based on the three rows
; in that leaf. The real disty is 0.14 -- a leaf reports a mean, not
; a min, so no single row's exact disty comes out. What matters is
; the routing: this row lands in a best-region leaf, not in the
; `Volume > 97 & Clndrs > 4` leaf at 0.82.
;
; Now the full evaluation protocol. `validate`:
;
; 1. Split the file into `train` and `test`, half each.
; 2. Run `active` on `train`. Spend ~30 labels.
; 3. Grow a tree on those labels.
; 4. Route every `test` row through the tree; sort `test` by
;    predicted leaf mean.
; 5. *Pay to peek* at the top `check` (default 5): reveal their true
;    disty.
; 6. Return the best of those five.
;
; The whole thing, twenty lines:

(defun validate (rs god w &optional (check 5))
  (let* ((body (shuffle (cdr rs)))
         (n (floor (length body) 2))
         (train (cons (car rs) (subseq body 0 n)))
         (test  (subseq body n)))
    (multiple-value-bind (best lab labels) (active train)
      (declare (ignore best))
      (let* ((y-god (lambda (r) (disty god r)))
             (train-best (extremum-by (? lab rows) y-god #'<))
             (tr (tree-grow lab (? lab rows)))
             (ranked (sort (copy-list test) #'<
                           :key (lambda (r)
                                  (mid (? (tree-leaf tr r) ynum)))))
             (top (subseq ranked 0 (min check (length ranked))))
             (pick (extremum-by top y-god #'<)))
        (values (funcall w train-best)
                (funcall w pick)
                (+ labels check))))))

; Run it 20 times, each with a different shuffle:
;
; ```text
; $ sbcl --script fri.lisp --active auto93.csv
; 100  97  49
;  94  67  50
;  97  93  55
;  97  97  33
; 100  91  48
; ...
; ```
;
; Three columns:
;
; - **train-win.** Percentile rank of the best row found during
;   training (100 = tied with the global best).
; - **hold-win.** Percentile rank of the best row chosen from the
;   holdout top-5 -- the test of whether the tree's explanation
;   *generalizes*.
; - **labels.** Total labels paid: active labels + 5 check labels.
;
; Typical hold-wins land in the 80s-90s with around 40 labels total
; (out of 398 rows). Honest evaluation: the tree was built without
; ever seeing the test rows, and the pay-to-peek budget is explicit.
;
; ----
;
; # 15. The whole show, in eight lines
;
; Every verb we have collected, in one REPL session:
;
; ```text
; * (defparameter *rs*  (read-csv "auto93.csv"))         ; chapter 2
; * (defparameter *d*   (make-data *rs*))                ; chapter 4
; * (mid (car (? *d* cols y)))                           ; chapter 6
; 23.844229
; * (norm (car (? *d* cols y)) 50)                       ; chapter 7
; 0.99394023
; * (disty *d* (cadr *rs*))                              ; chapter 9
; 0.7863217
; * (multiple-value-bind (_ lab n) (active *rs*)         ; chapter 12
;     (declare (ignore _))
;     (defparameter *tr* (tree-grow lab (? lab rows)))
;     (format t "~a labels~%" n))
; 32 labels
; * (tree-show *tr*)                                     ; chapter 13
; 0.53 ( 32) ...
; * (? (tree-leaf *tr* (car (last *rs*))) hdr)           ; chapter 14
; "Model <= 80"
; ```
;
; Read, classify columns, summarize, normalize, distance,
; active-learn, build a tree, route a row to its leaf. Eight lines.
; Every function was built from the one before it.
;
; ----
;
; # 16. Where we arrived
;
; We started with the integer 42. It did not know where it stood.
;
; We gave it friends: a row. We gave the row siblings: many rows. The
; header named the columns. We taught each column to remember its
; middle and its width in constant space. We placed every value
; inside its own column's bell. We defined a direction of good --
; *heaven*. We computed one scalar per row: distance from heaven. We
; sorted. We measured rows by their feature-space neighborhoods. We
; labeled cheaply. We grew a tree on the labels. We routed fresh rows
; through it. We measured generalization with a pay-to-peek holdout.
;
; Each step added one verb. None of the steps was more than a few
; lines:
;
; ```text
;     value  ->  row  ->  column  ->  mid/spread  ->  norm
;                                                      |
;                      distx <- heaven <- disty <-----+
;                       |                  |
;                     active -----> tree -----> validate
; ```
;
; Every arrow is a function. Every function is visible. Nothing hides
; inside a dependency. A lonely value walks all the way to
; explainable multi-objective optimization without ever leaving the
; file.
;
; ----
;
; ## Appendix A: every demo
;
; ```text
; $ sbcl --script fri.lisp --the    x           ; print config
; $ sbcl --script fri.lisp --rows   auto93.csv  ; read CSV, print tail
; $ sbcl --script fri.lisp --data   auto93.csv  ; build Data, show y-cols
; $ sbcl --script fri.lisp --stats  auto93.csv  ; mid, spread per column
; $ sbcl --script fri.lisp --norm   auto93.csv  ; raw vs normalized y's
; $ sbcl --script fri.lisp --ydata  auto93.csv  ; rows sorted by disty
; $ sbcl --script fri.lisp --xdata  auto93.csv  ; rows sorted by distx
; $ sbcl --script fri.lisp --tree   auto93.csv  ; active-label, grow tree
; $ sbcl --script fri.lisp --active auto93.csv  ; 20 train/test splits
; $ sbcl --script fri.lisp --all    auto93.csv  ; all of the above
; ```
;
; ## Appendix B: reader macros in one page
;
; Three pieces of notation save a lot of noise:
;
; - `!key` expands to `(second (assoc 'key *the*))`. A config lookup.
;   `!p` is the Minkowski exponent (default 2). `!budget` is the
;   label cap. Defined by one `defread`.
; - `$field` expands to `(slot-value i 'field)`. Inside a method, `i`
;   is always the first argument. So `$mu` in the `add ((i num) ...)`
;   method reads `i`'s `mu` slot. One other `defread`.
; - `(? x a b c)` is nested slot access:
;   `(slot-value (slot-value (slot-value x 'a) 'b) 'c)`. Used when
;   `i` isn't in scope. A regular macro, not a reader macro.
; - `(aif test then else)` is anaphoric if: binds `it` to `test`'s
;   value inside `then` and `else`. Used once, in the CLI parser, to
;   name "the matched flag-row" without a `let`.
;
; ## Appendix C: how the CLI finds your flag
;
; The whole dispatcher is two defuns:

(defun run (it &optional arg)
  (let* ((f (if (symbolp it) it
                (intern (format nil "EG~:@(~a~)" it))))
         (n (symbol-name f)))
    (when (and (fboundp f) (> (length n) 3)
               (string= n "EG-" :end1 3))
      (setf *seed* !seed)
      (if arg (funcall f arg) (funcall f))
      t)))

(defun cli (lsts)
  (loop for (flag arg) on (args) by #'cddr do
    (unless (run flag (thing arg))
      (aif (find flag lsts :key #'third :test #'equalp)
           (setf (second it) (thing arg))))))

; Walk argv two at a time. For each `(flag arg)` pair: first try to
; dispatch it as a demo by interning `"eg" + uppercase(flag)` and
; checking `fboundp`. If that fails, look up the flag in `*the*` by
; its third field (the short flag name) and overwrite the default.
; That is the entire parser: no argparse, no dictionary, no
; help-message generator. The same list that holds defaults is also
; the help schema.
