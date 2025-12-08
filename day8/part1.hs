import Data.Either (fromRight)
import Data.Function (on)
import Data.List
import Data.Ord
import Data.Text (pack, splitOn)
import Data.Text.Read qualified as R

-- NOTE: TOO slow
-- Solution 1: Try better Data structure (probably still not better)
-- Solution 2: For each circuit, keep track of the minimal distance of each other circuit
--  -> initial computation + for each connect you need to update for each circuit

main = do
  file <- readFile "test.txt"
  let circuits = parseInput file
  print $ multThreeLargestCircuits $ connect 10 circuits

-- print $ map length $ connect 4 circuits

data JunctionBox = JunctionBox Int Int Int
  deriving (Show, Eq, Ord)

type Circuit = [JunctionBox]

parseInput :: String -> [Circuit]
parseInput string = map (toList . toJunctionBox) splitCoords
 where
  toList x = [x]
  toJunctionBox [x, y, z] = JunctionBox (readInt x) (readInt y) (readInt z)
  splitCoords = map (splitOn delimiter) text
  text = map pack inputLines
  delimiter = pack ","
  inputLines = lines string

readInt x =
  case R.decimal x of
    Right (val, _) -> val
    Left _ -> error "invalid number"

boxDistance :: JunctionBox -> JunctionBox -> Float
boxDistance (JunctionBox x1 y1 z1) (JunctionBox x2 y2 z2) = sqrt $ xdiff ^ 2 + ydiff ^ 2 + zdiff ^ 2
 where
  xdiff = fromIntegral $ x1 - x2
  ydiff = fromIntegral $ y1 - y2
  zdiff = fromIntegral $ z1 - z2

distance :: Circuit -> Circuit -> Float
distance circuitA circuitB = minimum distances
 where
  distances = map (uncurry boxDistance) pairs
  pairs = [(c1, c2) | c1 <- circuitA, c2 <- circuitB]

multThreeLargestCircuits :: [Circuit] -> Int
multThreeLargestCircuits circuits = product largest
 where
  largest = (take 3 . sortBy (comparing Down) . map length) circuits

connect :: Int -> [Circuit] -> [Circuit]
connect 1 circuits = circuits -- connect nothing
connect n circuits = connect (n - 1) $ connectCircuits circuits

-- use data structure with fast concatenation and replacing of values
connectCircuits :: [Circuit] -> [Circuit]
connectCircuits circuits = (closestA ++ closestB) : filter (\c -> c /= closestA && c /= closestB) circuits
 where
  (closestA, closestB) = minimumBy (compare `on` uncurry distance) pairs
  pairs = [(c1, c2) | c1 <- circuits, c2 <- circuits, c1 /= c2]
