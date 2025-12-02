main = do
  file <- readFile "input.txt"
  let instructions = lines file
  print $ countPointingTo0 instructions

newtype Dial = Dial Int
  deriving (Show)

initDial :: Dial
initDial = Dial 50

type Instruction = String

pointsToZero :: Dial -> Bool
pointsToZero (Dial x) = x == 0

rotate :: Dial -> Instruction -> Dial
rotate (Dial x) (direction : count) = Dial wrapAroundValue
 where
  wrapAroundValue = (x `op` distance) `mod` 100
  distance = read count
  op = case direction of
    'L' -> (-)
    'R' -> (+)

countPointingTo0 :: [Instruction] -> Int
countPointingTo0 = fst . foldl accumulate (0, initDial)
 where
  accumulate (count, dial) instruction
    | pointsToZero newDial = (count + 1, newDial)
    | otherwise = (count, newDial)
   where
    newDial = rotate dial instruction
