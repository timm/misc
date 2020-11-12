#  col.gold
  - [Functions](#functions) 
    - [adds(i](#addsicolxstring) : Col, x
  - [Classes](#classes) 
    - [Col ](#colabstractclassforallcolumns) :  Abstract class for all columns.
      - [Col(i](#coliuntypedsstringnposint) : untyped, s
    - [Info ](#infocolumnswedonotsummarize) : columns we do not summarize
      - [Info(i](#infoiuntypedsstringinposint) : untyped, s
      - [Add(i](#addisymxatomatom) : Sym, x
    - [Sym ](#symmaintainssummariesofsymboliccolumns) :  maintains summaries of symbolic columns
      - [Sym(i](#symiuntypedsstringinposint) : untyped, s
      - [Add(i](#addisymxatomatom) : Sym, x
    - [Num](#nummaintainsummariesofnumericcolumns) :  maintain summaries of numeric columns,
      - [Num(i](#numiuntypedsstringnposint) : untyped, s
      - [_Pdf](#pdf) 
      - [_Cdf](#cdf) 
      - [_Crossover](#crossover) 

Summarize Columns


## Functions

### adds(i:Col, x:string)
Polymorphic update function for columns.

<details><summary>...</summary>

```awk
function add(i,x,  f) { f=i.is "Add"; return @f(i,x) }
```

</details>



## Classes

### Col : Abstract class for all columns.

#### Col(i:untyped, s:string, n:posint)
 Abstract constructor.
`s` is the name of a column appearing in positive `n`.

<details><summary>...</summary>

```awk
function Col(i,s,n) { 
  Object(i); i.is="Col"
  i.txt=s; i.pos=n }
```

</details>



### Info :columns we do not summarize

####  `Info(i:untyped, s:string,i n:posint)`
Constructor. 
`s` is the name of a column appearing in positive `n`.

<details><summary>...</summary>

```awk
function Info(i,s,n)  { Col(i,s,n); i.is="Info" }
```

</details>



#### Add(i:Sym, x:atom): atom
Do nothing.

<details><summary>...</summary>

```awk
function _Add(i,x) { return x }
```

</details>



### Sym : maintains summaries of symbolic columns

####  Sym(i:untyped, s:string,i n:posint)
Constructor.
`s` is the name of a column appearing in positive `n`.

<details><summary>...</summary>

```awk
function Sym(i,s,n) { 
  Col(i,s,n); i.is="Sym"
  i.mode= i.most= "" }
```

</details>



#### Add(i:Sym, x:atom): atom
Update frequency counts, and `mode`.

<details><summary>...</summary>

```awk
function _Add(i,x,   n) {
  if(x=="?") return x
  i.n++
  n= ++i.seen[x]
  if (n> i.most) { i.mode=x; i.most=n}
  return x }  
```

</details>



### Num: maintain summaries of numeric columns,

####  Num(i:untyped, s:string, n:posint)
Constructor.

<details><summary>...</summary>

```awk
function Num(i,s,n) { 
  Col(i,s,n); i.is="Num"
  i.hi = -1E32
  i.lo =  1E32
  i.mu= i.m2= i.n= i.sd=0 }
```

</details>



#### _Pdf
return height of the Gaussian at `x`
- i:Num
- x:number

<details><summary>...</summary>

```awk
function _Pdf(i,x,    var,denom,num) {
  var   = i.sd^2
  denom = (2*Au.pi*2*var)^.5
  num   = 2*Au.e^(-(x-i.mu)^2/(2*var+0.0001))
  return num/(denom + 10^-64) }
```

</details>



#### _Cdf
Return the area under the Gaussian from negative infinity to `x`.
- i:Num
- x:number

<details><summary>...</summary>

```awk
function _Cdf(i,x) { 
  x = (x-i.mu)/i.sd
  return (x<-3 || x>3) ? 0 : 1/(1+Au.e^(-0.07056*x^3 - 1.5976*x))}
```

</details>



#### _Crossover
Return where two Gaussians cross in-between their means.
- i:Num
- j:Num

<details><summary>...</summary>

```awk
function _Crossover(i,j,   x1,x2,d,min,x,y) {
   x1  = i.mu
   x2  = j.mu
   if (x2> x1) { x2=i.mu; x1=j.mu }
   d   = (x2-x1)/10
   min = 1E32
   for(x=x1; x<=x2; x+=d) {
      y = _Pdf(i) + _Pdf(j)
      if (y<min) { out=x; min = x} 
   } 
   return out }
```

</details>


