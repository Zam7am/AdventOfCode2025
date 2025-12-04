import Data.List

main = do
  file <- readFile "input.txt"
  let inputLines = lines file
  print $ totalJoltage inputLines

totalJoltage :: [String] -> Int
totalJoltage = sum . map maxJoltage

maxJoltage :: String -> Int
maxJoltage = maximum . possiblePairs

possiblePairs :: String -> [Int]
possiblePairs = map read . nub . possiblePairsWithDups

possiblePairsWithDups :: String -> [String]
possiblePairsWithDups "" = []
possiblePairsWithDups (c : cs) = pairs ++ possiblePairsWithDups cs
 where
  pairs = map (\c2 -> [c, c2]) cs
