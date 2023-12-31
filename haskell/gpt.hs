import qualified Data.Map as Map

-- Base type for numeric and symbolic columns
data Column = NUM { txt :: String, at :: Int, n :: Int, mu :: Double, m2 :: Double, hi :: Double, lo :: Double, heaven :: Int }
            | SYM { txt :: String, at :: Int, n :: Int, has :: Map.Map String Int, mode :: Maybe String, most :: Int }
            deriving (Show)

-- Constructors for NUM and SYM
newNUM :: String -> Int -> Column
newNUM s a = NUM s a 0 0 0 (-1e30) 1e30 (if "-" `elem` s then 0 else 1)

newSYM :: String -> Int -> Column
newSYM s a = SYM s a 0 Map.empty Nothing 0

-- Update functions for NUM
addNUM :: Double -> Column -> Column
addNUM x col@(NUM { n = n', mu = mu', m2 = m2', lo = lo', hi = hi' }) 
  | x /= "?" = let d = x - mu'
                   newMu = mu' + d / fromIntegral (n' + 1)
                   newM2 = m2' + d * (x - newMu)
               in col { n = n' + 1, mu = newMu, m2 = newM2, lo = min x lo', hi = max x hi' }
  | otherwise = col

-- Query functions for NUM
midNUM :: Column -> Double
midNUM (NUM { mu = mu' }) = mu'

divNUM :: Column -> Double
divNUM (NUM { n = n', m2 = m2' }) = if n' < 2 then 0 else sqrt (m2' / fromIntegral (n' - 1))

-- Update functions for SYM
addSYM :: String -> Column -> Column
addSYM x col@(SYM { n = n', has = has', mode = mode', most = most' })
  | x /= "?" = let newHas = Map.insertWith (+) x 1 has'
                   newMost = max most' (newHas Map.! x)
                   newMode = if newMost > most' then Just x else mode'
               in col { n = n' + 1, has = newHas, mode = newMode, most = newMost }
  | otherwise = col

-- Query functions for SYM
midSYM :: Column -> Maybe String
midSYM (SYM { mode = mode' }) = mode'

divSYM :: Column -> Double
divSYM (SYM { n = n', has = has' }) = let e = Map.foldr (\v acc -> acc - fromIntegral v / fromIntegral n' * logBase 2 (fromIntegral v / fromIntegral n')) 0 has'
                                      in e

-- Note: The '?' character handling, 'small' functions, and distance calculations are omitted as they require additional context from your Lua code.
