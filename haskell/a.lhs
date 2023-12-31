First code
==========

saasdasdsaads

> head (x:xs) =  x
> tail (x:xs)  =  xs

> inc n   = n + 1
> add a b = a + b

> fib x = if x<2 then 1 else fib (x - 1) + fib (x - 2)

> data Car = Car { company :: String ,model :: String, year :: Int } deriving(Show) 
> makeCar = Car{year=1980,company="ford",model="trhuster"}

> data BBTeam = BBTeam { teamname :: String, 
>                        manager :: Coach,
>                        players :: [BBPlayer] }  deriving (Show)

> data Coach = Coach { coachname :: String, 
>                      favcussword :: String,
>                      diet :: Diet }  deriving (Show)

> data Diet = Diet { dietname :: String, 
>                    steaks :: Integer, 
>                    eggs :: Integer }  deriving (Show)

> data BBPlayer = BBPlayer { playername :: String, 
>                            hits :: Integer,
>                            era :: Double }  deriving (Show)

> diets eggs = Diet{dietname="fred", steaks=1, eggs=eggs}

> addTEAM team = team {
>  manager = (manager team) {
>    diet = (diet (manager team)) {
>      steaks = steaks (diet (manager team)) + 1 }}}

> main :: IO ()
> main =  addTEAM 23
