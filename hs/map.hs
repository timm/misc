-- map :: (a -> b) -> [a] -> [b]
mop _ [] = []  
mop f (x:xs) = f x : mop f xs  

main = putStrLn "hi"
