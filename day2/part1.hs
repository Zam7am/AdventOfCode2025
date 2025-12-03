import Data.Char (isSpace, ord)

main = do
  file <- readFile "input.txt"
  let ranges = wordsWhen (== ',') file
  let newranges = map (filter (/= '\n')) ranges
  let intRanges = toIntRanges newranges
  print $ sum (countMirrors intRanges)
  return ()

countMirrors :: [[Int]] -> [Int]
countMirrors [] = []
countMirrors (xs : xss) = invalids ++ countMirrors xss
 where
  invalids = filter invalid xs
  invalid :: Int -> Bool
  invalid x = isEqualTuple (splitAt (length (show x) `div` 2) (show x))
  isEqualTuple (a, b) = a == b

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
   where
    (w, s'') = break p s'

toIntRanges :: [String] -> [[Int]]
toIntRanges = map toIntRange

toIntRange :: String -> [Int]
toIntRange range = [lower .. upper]
 where
  lower = read $ head rangeLimits
  upper = read (rangeLimits !! 1) + 1
  rangeLimits = wordsWhen (== '-') range

strip :: String -> String
strip str = stripped
 where
  strippedFront = dropWhile isSpace str
  stripped = reverse $ dropWhile isSpace (reverse strippedFront)
