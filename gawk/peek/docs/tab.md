#  tab.gold


- [Initialize columns in a table.](#initialize columns in a table.) Column names containing `?` become `Info` columns.
- [Add an row at some random index within `rows`.](#add an row at some random index within `rows`.) - i : Tab; table of data.
- [Copy the structure of `i` into a new table `j`.](#copy the structure of `i` into a new table `j`.) - i : Tab
## Row
### Constructor
      - [- i : untype](#- i : untype) 

_Header
Initialize columns in a table.
Column names containing `?` become `Info` columns.
Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
Dependent variables (stored in `ys`) are marked with `<>!` 
and all other are independent variables (stored in `xs`).
Klass names are marked in `!`.
- i : Tab
- a : array of column names.

<ul><details><summary><tt><tt>_Header()</tt></tt></summary>

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



_Data
Add an row at some random index within `rows`.
- i : Tab; table of data.
- a : array of data, to be read into the row.

<ul><details><summary><tt><tt>_Data()</tt></tt></summary>

```awk
function _Data(i,a,    r,j) {
  r = rand()
  has(i.rows, r, "Row")
  for(j=1; j<=length(a); j++) 
    i.rows[r].cells[j] = add(i.cols[j], a[j]) }
```

</details></ul>



_Clone
Copy the structure of `i` into a new table `j`.
- i : Tab
- j : untyped

<ul><details><summary><tt><tt>_Clone()</tt></tt></summary>

```awk
function _Clone(i,j) {
  Tab(j)
  TabHeader(j, i.names) }
```

</details></ul>




## Row

### Constructor

#### Row
- i : untype

<ul><details><summary><tt><tt>Row()</tt></tt></summary>

```awk
function Row(i) {
  Object(i)
  has(i,"cells")
  has(i,"ranges") }
```

</details></ul>




