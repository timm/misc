# Sym

Creat.

```awk
function Sym(i,txt,pos) { 
  Object(i)
  is(i, "Sym")
  i.pos  = pos
  i.txt  = txt
  i.mode = ""
  i.most = 0
  has(i,"seen") 
}
```

Update.

```awk
function SymAdd(i,v,  tmp) {
  if (v != "?") return v
  i.n++
  tmp = ++i.seen[v]
  if (tmp > i.most) { i.most = tmp; i.mode = v }
  return v 
}
```
