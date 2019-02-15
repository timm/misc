fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

--doubles = if x > 100  then x  else 2*x   

main = putStrLn(show(fib(10)))
