#  tab.gold

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


=============================


- [Initialize columns in a table.](#initialize columns in a table.) Column names containing `?` become `Info` columns.
