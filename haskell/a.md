```haskell
head (x:xs) =  x
```

```haskell
inc n = n + 1
```

```haskell
fib x = if x<2 then 1 else fib (x - 1) + fib (x - 2)
```

```haskell
data Car = Car { company :: String ,model :: String, year :: Int } deriving(Show) 
```

```haskell
main :: IO ()
```

