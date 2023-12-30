First code
==========

saasdasdsaads

```haskell
-- First item
head (x:xs) =  x
tail (x:xs)  =  xs
```

```haskell
inc n = n + 1
add a b = a + b
```

```haskell
fib x = if x<2 then 1 else fib (x - 1) + fib (x - 2)
```

```haskell
main :: IO ()
main = print(fib 10)
