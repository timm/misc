# Objects

```awk
BEGIN{  AU["oid"]  = 0
        AU["up"]   = ".."
        AU["dot"] = sprintf("%c",44)
}
function List(i)    { split("",i,"") }
function Object(i)  { List(i); i["is"] = "Object"; i["oid"] = ++AU["oid"] }

function zap(i,k)  { k = k?k:length(i)+1; i[k][0]; List(i[k]); return k } 

function has( i,k,f,      s) { f=f?f:"List"; s=zap(i,k); @f(i[k]);     return s}
function hass(i,k,f,m,    s) {               s=zap(i,k); @f(i[k],m);   return s}
function hasss(i,k,f,m,n, s) {               s=zap(i,k); @f(i[k],m,n); return s}

function inherit(k,f,   g) {
  while(k) {
    g = k f
    if (g in FUNCTAB) return g
    k = AU["is"][k]
  }
  print "#E> failed method lookup: ["f"]"
  exit 2
}
function is(i,x) {
  if ("is" in i) { AU["is"][x] = i["is"] }
  i["is"] = x
}
```
