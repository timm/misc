# Example

Define polymorphic verbs

```awk
function add(i,x, f) {f=i.is "Add"; @f(x,f) }
function mid(i,   f) {f=i.is "Mid"; @f(x) }
function var(i,   f) {f=i.is "Var"; @f(x) }
```

## Table

## Columns
### Column Ulities

```awk
function cols(a,b,  pat) {
  for(i in a)
    if (pat ~ i) b[i]
}
```

### Column Superclass

```awk
function Col(i,txt,pos) {
  Object(i)
  i.n   = 0
  i.txt = txt
  i.pos = pos
  i.w   = txt ~ /</ ? -1 : 1
}
```

### Num columns

```awk
function Num(i,txt,pos) {
  Col(i,txt,pos)
  i.is ="Num"
  i.mu = i.m2 = i.sd = 0
  i.lo = 10^32
  i.hi = -1*i.lo
  i.mu = i.m2 = i.sd = i.n = 0
}

function NumVar(i) { return i.sd }
function NumMid(i) { return i.mu }

function NumAdd(i,v,    d) {
  if (v=="?") return v
  v += 0
  i.n++
  i.lo  = v < i.lo ? v : i.lo
  i.hi  = v > i.hi ? v : i.hi
  d     = v - i.mu
  i.mu += d/i.n
  i.m2 += d*(v - i.mu)
  NumSd(i)
  return v
}
function NumSd(i) {
  if (i.m2 < 0) return 0
  if (i.n < 2)  return 0
  i.sd = (i.m2/(i.n - 1))^0.5
  return i.sd
}
```

### Sym columns

```awk
function Sym(i,txt,pos) {
  Col(i,txt,pos)
  i.is ="Sym"
  has(i,"all")
  i.mode = i.ent=  ""
  i.most = 0
}  

function SymVar(i) { return SymEnt(i) }
function SymMid(i) { return i.mode }

function SymAdd(i,v) {
  if (v=="?"p) return v
  i.ent=""
  i.n++
  i.all[v]++
  if (i.all[v] > i.most) {
    i.most = i.all[v]
    i.mode = v }
  return v
}
```
