import Data.List

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
parseInput input = [MathProblem (map read args) op | line <- transposedInput, let op = last line, let args = init line]
 where
  transposedInput = (transpose . map words . lines) input
