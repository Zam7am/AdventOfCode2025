import Data.List

main = do
  file <- readFile "input.txt"
  let ranges = parseInput file
  let minimisedRanges = minimiseRanges ranges
  print $ rangesCount minimisedRanges

parseInput :: String -> [Range]
parseInput input = map stringToRange ranges
 where
  stringToRange s = Range (lower s) (upper s)
  upper = read . tail . dropWhile (/= '-')
  lower = read . takeWhile (/= '-')
  ranges = filter ('-' `elem`) input_lines
  input_lines = lines input

-- Range lower upper (both inclusive)
data Range = Range Integer Integer
  deriving (Show)

rangesCount :: [Range] -> Integer
rangesCount = sum . map rangeCount

rangeCount :: Range -> Integer
rangeCount (Range lower upper) = (upper + 1) - lower

minimiseRanges :: [Range] -> [Range]
minimiseRanges = foldr minimise []

minimise :: Range -> [Range] -> [Range]
minimise range [] = [range]
minimise range ranges@(r : rs)
  | range `overlaps` r = minimise (range `merge` r) rs
  | otherwise = r : minimise range rs

merge :: Range -> Range -> Range
merge (Range lowerA upperA) (Range lowerB upperB) = Range newLower newUpper
 where
  newLower = min lowerA lowerB
  newUpper = max upperA upperB

overlaps :: Range -> Range -> Bool
overlaps (Range lowerA upperA) (Range lowerB upperB)
  | lowerA > upperB || lowerB > upperA = False
  | otherwise = True
