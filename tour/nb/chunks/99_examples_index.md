<!-- chunk: 99_examples_index -->
<!-- prev: 90_appendix_lua  next: (none) -->

## Appendix E. Examples index

Every entry below is one CLI flag, one line of Lua, and a captured
output. Together they are the executable summary of the program;
running `lua nb.lua --all-of-these` is the regression test.

### --the

```
$ lua nb.lua --the
{:k 1 :m 2 :wait 5}
```

### --sym

```
$ lua nb.lua --sym
SYM{:at 0 :has {:a 3 :b 1 :c 1} :n 5 :txt }
```

### --num

```
$ lua nb.lua --num
NUM{:at 0 :m2 500.00 :mu 25.00 :n 4 :sd 12.91 :txt }
```

### --col

```
$ lua nb.lua --col
NUM{:at 1 :m2 0 :mu 0 :n 0 :sd 0 :txt Age}	SYM{:at 2 :has {} :n 0 :txt name}
```

### --cols

```
$ lua nb.lua --cols
{NUM{:at 4 :m2 0 :mu 0 :n 0 :sd 0 :txt Class!}}
```

### --data

```
$ lua nb.lua --data data/diabetes.csv
SYM{:at 9 :has {:tested_negative 500 :tested_positive 268} :n 768 :txt class!}
```

### --like

```
$ lua nb.lua --like
0.025231325220202	0.58333333333333
```

### --likes

```
$ lua nb.lua --likes data/diabetes.csv
-28.309945819742
```

### --nb

```
$ lua nb.lua --nb data/diabetes.csv | wc -l
762

$ lua nb.lua --nb data/diabetes.csv | awk -F'\t' '$1==$2 {h++} END {print h}'
559
```

(559 correct out of 762 guesses — about 73% on a famously noisy
medical file. The same command on `data/soybean.csv` scores 576 out
of 677, about 85% on a cleaner multi-class file.)
