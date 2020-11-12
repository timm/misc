#  lib.gold


List 
initialize an empty list
- i:untyped

<details><summary>...</summary>

```awk
function List(i)        { split("",i,"") }
```

</details>



Object 
 Initialize a new object, give it a unique id (in `i.id`)
- i:untped

<details><summary>...</summary>

```awk
function Object(i)      { List(i); i.id = ++Au.id }
```

</details>



has 
Create something of class `f` inside of `i` at position `k`
- i:array
- k:atom  (string or number)
- f:?function name  (defaults to `List`).

<details><summary>...</summary>

```awk
function has(i,k,f)     { f=f?f:"List";i[k][0]; @f(i[k]);    delete i[k][0] }
```

</details>



haS
Like `has`, but accepts one constructor argument `x`.
- i:array
- k:atom  (string or number)
- f:function name  (defaults to `List`).
- x:any (something to be passed as `f(i,x)`)

<details><summary>...</summary>

```awk
function haS(i,k,f,x)   { i[k][0]; @f(i[k],x);   delete i[k][0] }
```

</details>



hAS
Like `has`, but accepts two constructor arguments `x` and `y`..
- i:array
- k:atom  (string or number)
- f:function name  (defaults to `List`).
- x:any (something to be passes as `f(i,x,y)`)
- y:any (something to be passed as `f(i,x,y)`)

<details><summary>...</summary>

```awk
function hAS(i,k,f,x,y) { i[k][0]; @f(i[k],x,y); delete i[k][0] }
```

</details>


