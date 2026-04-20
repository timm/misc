[index](index.md) · [1 motivation](01-motivation.md) · [2 landscape](02-shape.md) · [3 stats](03-stats.md) · [4 distance](04-distance.md) · [5 active](05-active.md) · [6 trees](06-trees.md) · [7 lisp](07-lisp.md) · [8 rejected](08-rejected.md) · [9 results](09-results.md)

# 3. Online stats

Chapter 2 built a vocabulary for representing data. This one builds the math for summarizing it.

The question: given a column of 400 numeric values, how do you answer "what is typical?" and "how spread out is the column?" without storing all 400 values in a list?

## The shape of a column

Plot a column of continuous values on a histogram. A curve emerges. Peaks where many rows cluster. Thin tails where few do. For most real columns, that curve is close to a bell. This is a *probability density function*, or PDF.

A PDF answers: how dense is the data near value `x`? A related function, the *cumulative density function* or CDF, answers: what fraction of the column is at or below `x`?

The bell is called a *Gaussian*. It is defined by two numbers. One (`mu`) says where the bell is centered. The other (`sigma`, the standard deviation) says how fat it is. Every Gaussian is just those two numbers.

So summarizing a column, to a first approximation, is computing `mu` and `sigma`. The PDF and CDF follow.

## The one-pass problem

A naive way to compute `mu`: sum every value, divide by count. That needs all the values in memory, or two passes over the file.

A naive way to compute `sigma`: sum of squared deviations from `mu`. That needs a pass to get `mu`, then another to get the deviations.

Welford solved both in 1962. You can maintain `mu` and a helper `m2` (the running sum of squared deviations from the *current* `mu`) with a single O(1) update per incoming value. After n values, `sigma = sqrt(m2 / (n - 1))`.

## Welford, in code

We already saw the update in Chapter 2. Here it is again with the edge case stripped, so the math is visible:

```lisp
(defmethod add ((i num) v &optional (w 1))
  (incf $n w)
  (let ((delta (- v $mu)))
    (incf $mu (float (/ (* w delta) $n)))
    (incf $m2 (float (* w delta (- v $mu))))))
```

Three increments per sample. No list of values. `mu` is always current; `m2` is always current; anything derived from them (SD, variance, z-score) costs O(1) to read. The `w` argument makes subtraction symmetric: pass `w = -1` and the same formulas unwind a previous add.

This is single-source-of-truth for the column. The summary is not built from stored rows on demand. The summary *is* the stored form. Rows can be discarded after passing through; the `Num` remembers what matters.

## Mid and spread

The bell has a center and a width. Every column type gives them different names but the same role.

```lisp
(defmethod mid ((i num)) $mu)
(defmethod mid ((i sym))
  (labels ((most (a b) (if (> (cdr a) (cdr b)) a b)))
    (car (reduce #'most $has))))
(defmethod mid ((i data))
  (or $mid (setf $mid (mapcar #'mid (? i cols all)))))
```

For a `Num`, mid is the mean. For a `Sym`, mid is the most frequent value (the mode). For a `Data`, mid is the centroid: a vector of mid values, one per column, memoized until the next `add`.

Spread is the same split:

```lisp
(defmethod spread ((i num))
  (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))
(defmethod spread ((i sym))
  (loop for (_ . v) in $has for p = (/ v $n) sum (- p (log p 2))))
```

For a `Num`, spread is the standard deviation, derived from `m2` and `n`. For a `Sym`, spread is an information measure over the frequency alist. Different math. Same role: "how much does this column vary?"

The reader never writes `if col.type == 'num'`. That test happens once, at method-dispatch time, because every `defmethod` is a polymorphic entry point. This is **OCP &mdash; Open/Closed Principle**: add a new column type (say, `date`) by writing new methods. No caller edits. No if-ladder grows by a branch.

## Normalization, or: putting everything on `[0, 1]`

Two columns have different scales. `Mpg` lives in 10..40. `Cylinders` lives in 3..8. You cannot add their deviations directly; the `Mpg` term will dominate. You need each column on a common scale first.

The standard trick: map a value to its z-score, then to its approximate CDF position. For a Gaussian, that is sigmoid of a scaled z:

```lisp
(defmethod norm ((i sym) v) v)
(defmethod norm ((i num) v)
  (if (eq v '?) v
      (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
        (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))
```

Three moves. Compute `z`, the z-score. Clamp to `[-3, 3]`, which covers about 99.7 percent of any Gaussian. Then sigmoid at scale 1.7, an approximation to the Gaussian CDF good enough for our purposes.

The output of `norm` on a `Num` is a probability: the approximate fraction of the column at or below `v`. Every column's values now live on the same `[0, 1]` scale. You can mix them, subtract them, average them.

A `Sym` value needs no scaling: comparing symbols means equal-or-not-equal. So `norm ((i sym) v)` is the identity.

## Why this matters

Chapter 4 needs to compute distances between rows. For that to work across columns of wildly different scales, the columns must already be normalizable in O(1). `norm` gives us that, because `spread` gives us SD in O(1), because Welford gave us `mu` and `m2` in O(1). Each piece supports the next.

Sixty lines of defmethod give us: online mean and variance, online entropy, a centroid cache, a normalization function, and polymorphism across three types. No `numpy`, no `scipy.stats`, no pre-allocated buffers.

## Try it yourself

**`--stats`.** Build a `Data` from a CSV. Walk every column. For each one, print its name, `mid`, and `spread`. The interesting part: your implementation should not compute mid and spread from the raw rows. It should read them from whatever the column object stored during `add`. If you find yourself iterating rows inside this command, something went wrong in Chapter 2.

**`--norm`.** Read a CSV, build a `Data`, pick the first row. For each `y` column, print the raw value and the normalized value. Numeric columns should map into `(0, 1)` via the sigmoid above. Symbolic columns should pass through unchanged. Verify that `norm(col, mu(col))` is close to `0.5` &mdash; the center of the CDF.

The combined exercise: a one-pass pipeline that reads 400 rows, summarizes 8 columns online, and hands you per-column mid, spread, and a normalizer. In Python, about twenty lines on top of Chapter 2. None of it needs a stats library.
