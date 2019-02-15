fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

doubles x = if x > 100  then x  else 2*x   

rightTri = [ (a,b,c) | c <- [1..10], 
                              b <- [1..c], 
                              a <- [1..b], 
                              a^2 + b^2 == c^2, a+b+c == 24]  
-- main = putStrLn(show(doubles(fib(10))))
main = putStrLn(show(rightTri))

