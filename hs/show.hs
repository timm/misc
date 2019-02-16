data BBTeam = BBTeam { teamname :: String, 
                       manager :: Coach,
                       players :: [BBPlayer] }  
     deriving (Show)

data Coach = Coach { coachname :: String, 
                     favcussword :: String,
                     diet :: Diet }  
     deriving (Show)

data Diet = Diet { dietname :: String, 
                   steaks :: Integer, 
                   eggs :: Integer }  
     deriving (Show)

data BBPlayer = BBPlayer { playername :: String, 
                           hits :: Integer,
                           era :: Double }  
     deriving (Show)

