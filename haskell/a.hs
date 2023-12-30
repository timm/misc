head (x:xs) =  x
tail (x:xs)  =  xs

inc n = n + 1
add a b = a + b

fib x = if x<2 then 1 else fib (x - 1) + fib (x - 2)

main :: IO ()
main = print(fib 10)