
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float 
           | Rectangle Point Point deriving (Show)

surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) 
                   (Point x2 y2)) = 
  (abs $ x2 - x1) * (abs $ y2 - y1)  

c = Circle (Point 2 2)   2.0
r = Rectangle (Point 1 2) (Point 10 20)

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) 

data Dog = Dog { name :: String } deriving (Show)
data Cat = Cat {  aged :: Int
               ,  nome :: String } deriving (Show)

--Dog = Dog { name :: String 
--          } deriving (Show)

-- Cat = Cat { name :: String, age :: Int } deriving (Show)

data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)  

car0 = Car{model="Ford", company="IBM", year=2000}

tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {model = c, company = m, year = y}) = 
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  

main = print(tellCar( car0 {year=2030}))

--main = putStrLn(show( 10))
