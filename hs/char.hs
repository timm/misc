import Data.Char

readInts :: IO ()
readInts = do
  putStrLn "Choose nr of ints to sum."
  c <- getChar
  let i = digitToInt c
  let sum = getInts i
  let str = "\nSum is: " ++ [intToDigit i].
  putStrLn str

main = putStrLn "1"
