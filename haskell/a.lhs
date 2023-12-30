#!/usr/bin/env runghc 

First code
==========

saasdasdsaads

> head (x:xs) =  x
> tail (x:xs)  =  xs

> inc n = n + 1
> add a b = a + b

> fib x = if x<2 then 1 else fib (x - 1) + fib (x - 2)

> data Car = Car { company :: String ,model :: String, year :: Int } deriving(Show) 

> makeCar = Car{year=1980,company="ford",model="trhuster"}

> main :: IO ()
> main = print(makeCar{year=2})
