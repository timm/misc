fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

doubles x = if x > 100  then x  else 2*x   

rightTri = [ (a,b,c) | c <- [1..10], 
                              b <- [1..c], 
                              a <- [1..b], 
                              a^2 + b^2 == c^2] -- a+b+c < 24]  

demo = read "5" :: Float 

downCase st = [ c | c <- st, not (elem c  ['A'..'Z'])] 

data Employee = Employee { name :: String, salary :: Double }
bob   = Employee { name = "Bob", salary = 1000000 }

data Question = Question{ answered :: Bool
                        , text     :: String }

mop _ [] = []  
mop f (x:xs) = f x : mop f xs  

quicksort [] = [] 
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

m=Question True "asd"
moin =  do 
  putStrLn(show(doubles(fib 10)))
  putStrLn(show(doubles(fib 10)))

main =  do 
  filter (<=2) [1,2,3,4]

--  quicksort([10,1, 5 ,7, 2, 8, 3])
--main = putStrLn(show(bob))

