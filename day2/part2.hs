import Data.Char (isSpace, ord)
import Data.List (inits, isPrefixOf, stripPrefix, tails)
import Data.Maybe (fromJust)

main = do
  file <- readFile "input.txt"
  let ranges = wordsWhen (== ',') file
  let newranges = map (filter (/= '\n')) ranges
  let intRanges = toIntRanges newranges
  print $ sum (getInvalidIDs intRanges)

getInvalidIDs :: [[Int]] -> [Int]
getInvalidIDs [] = []
getInvalidIDs (xs : xss) = invalids ++ getInvalidIDs xss
 where
  invalids = filter invalid xs

invalid :: Int -> Bool
invalid x = any (`isRepeatsOf` str) substr
 where
  str = show x
  possiblePatterns = filter (\s -> stringlen `mod` length s == 0) substr -- just to improve efficiency slightly
  substr = filter (/= str) $ substrings str
  stringlen = length str

isRepeatsOf :: String -> String -> Bool
isRepeatsOf pattern [] = True
isRepeatsOf pattern str
  | pattern `isPrefixOf` str = isRepeatsOf pattern $ (fromJust . stripPrefix pattern) str -- guaranteed due to isPrefixOf check
  | otherwise = False

substrings :: String -> [String]
substrings s = concatMap (filter (not . null) . inits) (tails s)

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
