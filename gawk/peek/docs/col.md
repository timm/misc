#  col.gold
  - [Functions)](#functions) 
    - [add)](#add) 
  - [Classes)](#classes) 
    - [Col : Abstract class for all columns.)](#colabstractclassforallcolumns) 
      - [Col)](#col) 
    - [Info)](#info) 
      - [Info)](#info) 
      - [_Add)](#add) 
    - [Sym)](#sym) 
      - [Sym)](#sym) 
      - [_Add)](#add) 
    - [Num)](#num) 
      - [Num)](#num) 
      - [_Add)](#add) 
      - [_Pdf)](#pdf) 
      - [_Cdf)](#cdf) 
      - [_Crossover)](#crossover) 

## Functions
Polymorphic update function for columns.
### add
`add(i:Col, x:string)`

Polymorphic update function for columns.

<ul><details><summary>...</summary>

```awk
function add(i:Col,x:string,  f) { f=i.is "Add"; return @f(i,x) }
```

</details></ul>


## Classes
### Col : Abstract class for all columns.
 Abstract constructor.
`s` is the name of a column appearing in positive `n`.
#### Col
`Col(i:untyped)`

 Abstract constructor.
`s` is the name of a column appearing in positive `n`.

<ul><details><summary>...</summary>

```awk
function Col(i:untyped, s:string, n:posint) { 
  Object(i); i.is="Col"
  i.txt=s; i.pos=n }
```

</details></ul>


### Info 
Constructor. 
`s` is the name of a column appearing in positive `n`.
#### Info
`Info(i:untyped)`

Constructor. 
`s` is the name of a column appearing in positive `n`.

<ul><details><summary>...</summary>

```awk
function Info(i:untyped, s:string, n:posint)  { 
   Col(i,s,n); i.is="Info" }
```

</details></ul>


Do nothing.
#### _Add
`_Add(i:Sym)`

Do nothing.

<ul><details><summary>...</summary>

```awk
function _Add(i:Sym, x:any) {return x}
```

</details></ul>


### Sym 
Constructor.
`s` is the name of a column appearing in positive `n`.
#### Sym
`Sym(i:untyped)`

Constructor.
`s` is the name of a column appearing in positive `n`.

<ul><details><summary>...</summary>

```awk
function Sym(i:untyped, s:string, n:posint) { 
  Col(i,s,n); i.is="Sym"
  i.mode= i.most= "" }
```

</details></ul>


Update frequency counts, and `mode`.
#### _Add
`_Add(i:Sym)`

Update frequency counts, and `mode`.

<ul><details><summary>...</summary>

```awk
function _Add(i:Sym, x:atom,    n) {
  if(x=="?") return x
  i.n++
  n= ++i.seen[x]
  if (n> i.most) { i.mode=x; i.most=n}
  return x }  
```

</details></ul>


### Num
Constructor.
#### Num
`Num(i:untyped)`

Constructor.

<ul><details><summary>...</summary>

```awk
function Num(i:untyped, s:string, n:posint) { 
  Col(i,s,n); i.is="Num"
  i.w  = (s ~ /</) ? -1 : 1 
  i.hi = -1E32
  i.lo =  1E32
  i.mu = i.m2= i.n= i.sd=0 }
```

</details></ul>


Update self, return `x`.
#### _Add
`_Add(i:Num)`

Update self, return `x`.

<ul><details><summary>...</summary>

```awk
function _Add(i:Num, x:number,    d) {
  if(x=="?") return x
  i.n++
  if(x > i.hi) i.hi = x
  if(x < i.lo) i.lo = x
  d     = x - i.mu
  i.mu += d / i.n
  i.m2 += d * (x - i.mu) 
  i.sd  = (i.n<2 || i.m2<0) ? 0 : i.sd = (i.m2/(i.n-1))^0.5
  return x }
```

</details></ul>


Return height of the Gaussian at `x`.
#### _Pdf
`_Pdf(i:Num)`

Return height of the Gaussian at `x`.

<ul><details><summary>...</summary>

```awk
function _Pdf(i:Num, x:any,    var,denom,num) {
  var   = i.sd^2
  denom = (2*Au.pi*2*var)^.5
  num   = 2*Au.e^(-(x-i.mu)^2/(2*var+0.0001))
  return num/(denom + 10^-64) }
```

</details></ul>


Return the area under the Gaussian from negative infinity to `x`.
#### _Cdf
`_Cdf(i:Num)`

Return the area under the Gaussian from negative infinity to `x`.

<ul><details><summary>...</summary>

```awk
function _Cdf(i:Num, x:number) { 
  x = (x-i.mu)/i.sd
  return (x<-3 || x>3) ? 0 : 1/(1+Au.e^(-0.07056*x^3 - 1.5976*x))}
```

</details></ul>


Return where two Gaussians cross in-between their means.
#### _Crossover
`_Crossover(i:Num, j:Num)`

Return where two Gaussians cross in-between their means.

<ul><details><summary>...</summary>

```awk
function _Crossover(i:Num,j:Num,   x1,x2,d,min,x,y) {
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
