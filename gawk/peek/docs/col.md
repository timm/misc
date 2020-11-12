#  col.gold
  - [Functions](#functions) 
    - [adds](#adds) 
  - [Classes](#classes) 
    - [Col](#col) 
    - [Info](#info) 
    - [Sym](#sym) 
    - [Num](#numclassfornumericw) :  class for numeric
      - [_Pdf](#pdf) 
      - [_Cdf](#cdf) 
      - [_Crossover](#crossover) 

Summarize Columns


## Functions

### adds
Polymorphic update function for columns.
- i:Col
- x:string or number

<details><summary>...</summary>

```awk
function add(i,x,  f) { f=i.is "Add"; return @f(i,x) }
```

</details>



## Classes

### Col
 abstract class for all columns
- i:untyped
- s:string (name of column)
- n:posint (index of column)

<details><summary>...</summary>

```awk
function Col(i,s,n) { 
  Object(i); i.is="Col"
  i.txt=s; i.pos=n }
```

</details>



###  Info
Class  for columns we do not summarize.
- i:untyped
- s:string (name of column)
- n:posint (index of column)

<details><summary>...</summary>

```awk
function Info(i,s,n)  { Col(i,s,n); i.is="Info" }
function _Add(i,x) { return x }
```

</details>



### Sym
Class for symbolic values.

<details><summary>...</summary>

```awk
function Sym(i,s,n) { 
  Col(i,s,n); i.is="Sym"
  i.mode= i.most= "" }
```

</details>



### Num: class for numeric:w

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


