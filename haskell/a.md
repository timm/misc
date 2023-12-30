First code
==========

saasdasdsaads

```haskell
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
data Car = Car { company :: String ,model :: String, year :: Int } deriving(Show) 
makeCar = Car{year=1980,company="ford",model="trhuster"}
```

```haskell
main :: IO ()
main = print(makeCar{year=2})
```

