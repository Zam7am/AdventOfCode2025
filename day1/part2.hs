main = do
  file <- readFile "input.txt"
  let instructions = lines file
  print $ countZeros initDial instructions

-- Dial with pointer and count of times its pointer pointed to 0
data Dial = Dial Int Int
  deriving (Show)

initDial :: Dial
initDial = Dial 50 0

type Instruction = String

countZeros :: Dial -> [Instruction] -> Int
countZeros (Dial _ count) [] = count
countZeros dial (i : is) = countZeros (turnDial dial i) is

turnDial :: Dial -> Instruction -> Dial
turnDial dial ('R' : count) = turnRight dial (read count)
turnDial dial ('L' : count) = turnLeft dial (read count)

turnRight :: Dial -> Int -> Dial
turnRight dial 0 = dial
turnRight (Dial pointer zeroCount) turnCount
  | pointer == 99 = turnRight (Dial 0 (zeroCount + 1)) (turnCount - 1)
  | otherwise = turnRight (Dial (pointer + 1) zeroCount) (turnCount - 1)

turnLeft :: Dial -> Int -> Dial
turnLeft dial 0 = dial
turnLeft (Dial pointer zeroCount) turnCount
  | pointer == 1 = turnLeft (Dial 0 (zeroCount + 1)) (turnCount - 1)
  | pointer == 0 = turnLeft (Dial 99 zeroCount) (turnCount - 1)
  | otherwise = turnLeft (Dial (pointer - 1) zeroCount) (turnCount - 1)
