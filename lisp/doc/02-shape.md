[index](index.md) · [1 motivation](01-motivation.md) · [2 landscape](02-shape.md) · [3 stats](03-stats.md) · [4 distance](04-distance.md) · [5 active](05-active.md) · [6 trees](06-trees.md) · [7 lisp](07-lisp.md) · [8 rejected](08-rejected.md) · [9 results](09-results.md)

# 2. The landscape

The field is called *data* mining. Most textbooks ignore the data.

They race to algorithms. Decision trees, gradient descent, backprop, transformers. Each one works only because the input has regularities: correlations, clusters, separable classes, distributions that repeat themselves. If the rows were uniform noise, no algorithm would help. So what shape is the data?

This chapter builds the vocabulary for talking about that shape. No learner touches the data until Chapter 5. For now we make the landscape visible.

## A row has mixed parts

Some values are numbers. Some are words. Some columns are features we observe; others are goals we want to optimize. The first regularity is the type distinction. Two struct types cover it:

```lisp
(defstruct sym (at 0) (txt " ") (n 0) has)

(defstruct (num  (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (heaven 1))
```

Both carry a position (`at`), a name (`txt`), a count (`n`). A `Sym` keeps a frequency alist in `has`. A `Num` keeps Welford's running mean (`mu`) and sum of squared deviations (`m2`). `heaven` is the direction of "good": 1 for maximize, 0 for minimize.

Two types, because one does not fit both. Adding "five" and "three" makes no sense. Adding 5.0 and 3.0 does. Asking for the mean of `["red", "green", "blue"]` is a category error. Asking for the mode is not. Keep the types separate and the code does not have to branch.

## Column names carry roles

Beyond num/sym, another regularity. A column's name tells you whether it is input or output, goal or feature. Our CSV format makes the rule explicit: capitalize numerics, suffix goal columns with `+` or `-`, mark a class with `!`, mark skipped columns with `X`.

```lisp
(defstruct (cols (:constructor %make-cols)) names x y all)
```

The classifier:

```lisp
(defun make-cols (txts &aux (i (%make-cols :names txts)) (n -1))
  (labels ((ako (txt)
             (if (upper-case-p (ch txt 0)) #'make-num #'make-sym))
           (one (txt) (funcall (ako txt) :txt txt :at (incf n)))
           (end (col) (ch (? col txt) -1)))
    (setf $all (mapcar #'one txts))
    (dolist (col $all i)
      (unless (eql (end col) #\X)
        (if (find (end col) "!-+") (push col $y) (push col $x))))))
```

Capital first letter means `Num`. Lowercase means `Sym`. Last character sets the role. A column named `weightX` is skipped entirely. Convention over configuration: the data tells us what the data is. No registry, no manifest, no plugin file.

## Data is rows plus their summary

```lisp
(defstruct (data (:constructor %make-data)) rows cols mid)
```

One idea drives the design: *the stats are the data.* You do not keep rows in one place and a table of statistics in another. Every column object in `Cols.all` carries its own running summary. `mu` updates as rows arrive. Ask for the mean; read `mu`. There is no separate "fit" step.

This is single-source-of-truth by construction. Each row contributes once to exactly one set of summaries. Pop the row with `sub` and the summaries correct themselves.

## Four verbs move the data

Every mutation in the codebase is one of four operations. The same four every REST API pretends to have.

**Create.** `add` pushes a value into a column's summary and returns the value:

```lisp
(defmethod add ((i num) v &optional (w 1))
  (unless (eq v '?)
    (cond ((and (minusp w) (<= $n 2))
           (setf $n 0 $mu 0 $m2 0))
          (t (incf $n w)
             (let ((delta (- v $mu)))
               (incf $mu (float (/ (* w delta) $n)))
               (incf $m2 (float (* w delta (- v $mu))))))))
  v)
```

Welford's online update, one `incf` per term. Missing values (`?`) skip. The `cond` guards one numerical edge case when a subtraction would push `n` below two; the column resets rather than go unstable.

The `Sym` method has the same shape, simpler inside:

```lisp
(defmethod add ((i sym) v &optional (w 1))
  (unless (eql v '?)
    (incf $n w)
    (let ((cell (assoc v $has :test #'equal)))
      (if cell
          (incf (cdr cell) w)
          (push (cons v w) $has))))
  v)
```

One call site, two types, no `if column.type == "num":`. `defmethod` dispatches.

**Delete.** `sub` is `add` at weight -1:

```lisp
(defun sub (it v) (add it v -1))
```

Subtraction works because Welford is symmetric. Removing a row is as cheap as adding one. Chapter 5 uses this to move rows between `best` and `rest` sets without rebuilding anything.

**Read.** The `$field` reader macro expands to a slot access:

```lisp
$mu  ;; expands to  (slot-value i 'mu)
```

No accessor functions. No `num-mu`, `num-n`, `sym-has` cluttering the namespace.

**Update.** `setf` on the slot. That is the whole API.

## Adding a row

Put a row into a `Data` and watch dispatch thread through three layers:

```lisp
(defmethod add ((i data) row &optional (w 1))
  (setf $mid nil)
  (add $cols row w)
  (if (plusp w)
      (push row $rows)
      (setf $rows (remove row $rows :test #'equal :count 1)))
  row)

(defmethod add ((i cols) row &optional (w 1))
  (mapcar (lambda (col v) (add col v w)) $all row)
  row)
```

One call on the `Data`. It recurses into the `Cols`. That maps across every column, dispatching to either the `Num` or `Sym` method. The cached `mid` is invalidated. The row is pushed onto the list.

No type switches at any call site. The machinery knows.

## What we got

Forty lines of struct and defmethod give us a running summary of any CSV, row removal as cheap as row addition, role assignment from column names alone, and a type system that dispatches for us. No SQL, no pandas, no ORM. The data carries its own schema.

## Try it yourself

**`--rows`.** Read a CSV file into a list of rows. Each row is a list of values. Strings that parse as numbers become numbers. The literal string `"?"` becomes a distinct missing-value marker. Everything else stays a string. Handle the header row like any other. In Python: about fifteen lines, no imports beyond `csv` or a manual split on commas. The exercise is noticing how little parsing you actually need.

**`--data`.** Take the list of rows. Treat the first as the header. For each header string, build either a `Num` or `Sym` based on the first character and assign it to `x` (feature), `y` (goal), or skip (`X`). Feed every subsequent row through each column so its running summary updates. Return an object that holds both the rows and the column summaries. Print the `y` columns to show you got the classification right.

**`--the`.** (From Chapter 1 if you skipped it.) A config list of four-tuples: `(name default flag description)`. A tiny CLI parser walks `argv`, matches flags, overwrites defaults. Your language's equivalent of argparse should take five minutes to out-golf.

The combined exercise so far: read a CSV, summarize its columns by a single online pass, classify features from goals by naming convention. Total code in Python: maybe eighty lines. You now own every abstraction the rest of the book builds on.
