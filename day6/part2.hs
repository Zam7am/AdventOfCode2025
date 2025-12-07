import Data.Char
import Data.List (transpose, words)

main = do
  file <- readFile "input.txt"
  let problems = parseInput file
  print $ sum $ map computeMath problems

data MathProblem = MathProblem [Int] Op
  deriving (Show)

type Op = String

computeMath :: MathProblem -> Int
computeMath (MathProblem operands "+") = sum operands
computeMath (MathProblem operands "*") = product operands

parseInput :: String -> [MathProblem]
parseInput input = [MathProblem (map read operands) op | (operands, op) <- zip cephalopodStyle ops]
 where
  cephalopodStyle = splitIf (all isSpace) $ transpose operandLines -- transpose results in whitespace strings between each expression
  ops = (words . head) opLines -- only one line with ops
  (operandLines, opLines) = splitAt (length inputLines - 1) inputLines
  inputLines = lines input

splitIf :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitIf predicate xs = case dropWhile predicate xs of
  [] -> []
  xs' -> prefix : splitIf predicate xs''
   where
    (prefix, xs'') = break predicate xs'
