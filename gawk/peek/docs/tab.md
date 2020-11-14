#  tab.gold
  - [Row](#row) : ### Constructor
      - [Row](#row) : Initialize columns in a table.

Initialize columns in a table.
Column names containing `?` become `Info` columns.
Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
Dependent variables (stored in `ys`) are marked with `<>!` 
and all other are independent variables (stored in `xs`).
Klass names are marked in `!`.
- i : Tab
- a : array of column names.
_Header
Initialize columns in a table.
Column names containing `?` become `Info` columns.
Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
Dependent variables (stored in `ys`) are marked with `<>!` 
and all other are independent variables (stored in `xs`).
Klass names are marked in `!`.
- i : Tab
- a : array of column names.

<ul><details><summary><tt>_Header()</tt></summary>

```awk
function _Header(i,a,   where, what, j) {
  for(j=1; j<=length(a); j++) {
    i.names[j] = a[j]
    if (a[j] ~ /\?/) {
      what="Info"
      where="info"
    } else {
      what = a[j] ~ /[:<>]/ ?  "Num" : "Sym"
      where= a[j] ~ /[!<>]/ ?  "ys"  : "xs"
    }
    hAS(i.cols, j, what, a[j],j)   
    i[where][j]
    if (a[j]~/!/) i.klass = j }}
```

</details></ul>


Add an row at some random index within `rows`.
- i : Tab; table of data.
- a : array of data, to be read into the row.
_Data
Initialize columns in a table.
Column names containing `?` become `Info` columns.
Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
Dependent variables (stored in `ys`) are marked with `<>!` 
and all other are independent variables (stored in `xs`).
Klass names are marked in `!`.
- i : Tab
- a : array of column names.
Add an row at some random index within `rows`.
- i : Tab; table of data.
- a : array of data, to be read into the row.

<ul><details><summary><tt>_Data()</tt></summary>

```awk
function _Data(i,a,    r,j) {
  r = rand()
  has(i.rows, r, "Row")
  for(j=1; j<=length(a); j++) 
    i.rows[r].cells[j] = add(i.cols[j], a[j]) }
```

</details></ul>


Copy the structure of `i` into a new table `j`.
- i : Tab
- j : untyped
_Clone
Initialize columns in a table.
Column names containing `?` become `Info` columns.
Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
Dependent variables (stored in `ys`) are marked with `<>!` 
and all other are independent variables (stored in `xs`).
Klass names are marked in `!`.
- i : Tab
- a : array of column names.
Add an row at some random index within `rows`.
- i : Tab; table of data.
- a : array of data, to be read into the row.
Copy the structure of `i` into a new table `j`.
- i : Tab
- j : untyped

<ul><details><summary><tt>_Clone()</tt></summary>

```awk
function _Clone(i,j) {
  Tab(j)
  TabHeader(j, i.names) }
```

</details></ul>


## Row
### Constructor
- i : untype
#### Row
Initialize columns in a table.
Column names containing `?` become `Info` columns.
Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
Dependent variables (stored in `ys`) are marked with `<>!` 
and all other are independent variables (stored in `xs`).
Klass names are marked in `!`.
- i : Tab
- a : array of column names.
Add an row at some random index within `rows`.
- i : Tab; table of data.
- a : array of data, to be read into the row.
Copy the structure of `i` into a new table `j`.
- i : Tab
- j : untyped
- i : untype

<ul><details><summary><tt>Row()</tt></summary>

```awk
function Row(i) {
  Object(i)
  has(i,"cells")
  has(i,"ranges") }
```

</details></ul>


