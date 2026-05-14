<!-- chunk: 02_peek -->
<!-- prev: 01_motivation  next: 03_install -->

## A peek at what we are building

The whole program is one CSV in, one stream of guesses out. The CSV
has a header row that names the columns. A trailing `!` on a name
marks that column as the class label. Here are the first three lines
of a diabetes dataset:

```
PREG,PLAS,PRES,SKIN,INSU,MASS,PEDI,AGE,class!
6,148,72,35,0,33.6,0.627,50,tested_positive
1,85,66,29,0,26.6,0.351,31,tested_negative
```

Run the classifier on the whole file:

```
$ lua nb.lua --nb data/diabetes.csv
tested_positive	tested_positive
tested_positive	tested_negative
tested_positive	tested_positive
tested_negative	tested_negative
...
```

Each line is two values: what the classifier guessed, and what the
row actually was. After 768 rows the program has read the file once
and produced 762 guesses (it waits five rows before it starts
guessing — we will see why). Counting matches on this run gives:

```
guessed=negative actual=negative  403
guessed=positive actual=positive  156
guessed=positive actual=negative  109
guessed=negative actual=positive   94
```

That is about 73% correct on a famously noisy medical dataset. We
will look at where each digit of that number comes from.
