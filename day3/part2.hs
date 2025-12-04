import Data.List

main = do
  file <- readFile "input.txt"
  let inputLines = lines file
  print $ sum (maxJoltages inputLines)

maxJoltages :: [String] -> [Integer]
maxJoltages = map maxJoltage

maxJoltage :: String -> Integer
maxJoltage digits = maxJoltage' prefix suffix
 where
  (prefix, suffix) = splitAt 12 digits

maxJoltage' :: String -> String -> Integer
maxJoltage' maxElem "" = read maxElem
maxJoltage' maxElem (digit : digits) = maxJoltage' newMax digits
 where
  newMax = show $ maximum $ numberWithDigitCombinations maxElem digit

numberWithDigitCombinations :: String -> Char -> [Integer]
numberWithDigitCombinations number digit = oldNumber : map read combinations
 where
  combinations = map (`combineChar` digit) [0 .. 11]
  combineChar idx char = remove number idx ++ [char]
  oldNumber = read number

remove :: [a] -> Int -> [a]
remove xs idx = take idx xs ++ drop (1 + idx) xs
