import Data.List

main = do
  file <- readFile "input.txt"
  let (ranges, ingredients) = parseInput file
  print $ length $ validIngredients ranges ingredients

-- Range Lower Upper
data Range = Range Integer Integer
  deriving (Show)

type Ingredient = Integer

parseInput :: String -> ([Range], [Ingredient])
parseInput input = (map stringToRange ranges, map read ingredients)
 where
  stringToRange s = Range (lower s) (upper s)
  upper = read . tail . dropWhile (/= '-')
  lower = read . takeWhile (/= '-')
  (ranges, ingredients) = partition ('-' `elem`) input_lines
  input_lines = filter (/= "") $ lines input

validIngredients :: [Range] -> [Ingredient] -> [Ingredient]
validIngredients ranges = filter inRanges
 where
  inRanges ingredient = any (ingredient `inRange`) ranges
  inRange ingredient (Range lower upper) = lower <= ingredient && ingredient <= upper
