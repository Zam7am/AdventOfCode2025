import Data.Heap (nub)
import Data.Heap qualified as Heap
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Ord qualified as Ord
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Read
import Debug.Trace (trace)

main = do
  file <- readFile "input.txt"
  let junctionBoxes = parseInput file
  let edges = possibleEdges junctionBoxes
  let pairs = take 1000 $ shortestEdges edges
  let circuits = makeCircuits pairs
  print circuits
  print $ multLargestCircuits circuits

multLargestCircuits :: Set.Set Circuit -> Int
multLargestCircuits circuits = product (take 3 (map length sorted))
 where
  sorted = (List.sortOn (Ord.Down . length) . Set.elems) circuits

makeCircuits :: [Edge] -> Set.Set Circuit
makeCircuits = foldr insertIntoCircuit Set.empty

insertIntoCircuit :: Edge -> Set.Set Circuit -> Set.Set Circuit
insertIntoCircuit (Edge (i, j) _) circuits =
  case overlapping of
    [] -> Set.insert nodes circuits
    [intersectingCircuit] -> updateCircuit (`Set.union` nodes) intersectingCircuit circuits
 where
  overlapping = take 1 $ Set.elems $ Set.filter (`overlaps` nodes) circuits
  nodes = Set.fromList [i, j]

updateCircuit :: (Circuit -> Circuit) -> Circuit -> Set.Set Circuit -> Set.Set Circuit
updateCircuit mapper circuit circuits = mergeOverlapping updated
 where
  updated = Set.insert (mapper circuit) $ Set.delete circuit circuits

mergeOverlapping :: Set.Set Circuit -> Set.Set Circuit
mergeOverlapping circuits
  | any (\c -> any (c `overlaps`) circuits) circuits = Set.delete circuit newSet
  | otherwise = newSet
 where
  newSet = Set.insert merged cleanedSet
  cleanedSet = foldr Set.delete circuits overlapping
  merged = Set.foldr Set.union Set.empty overlapping
  overlapping = Set.filter (`overlaps` circuit) circuits

shortestEdges :: Edges -> [Edge]
shortestEdges (Edges edges)
  | Heap.null edges = []
  | otherwise = shortest : shortestEdges otherEdges
 where
  otherEdges = Edges (Heap.deleteMin edges)
  shortest = Heap.minimum edges

parseInput :: String -> [JunctionBox]
parseInput input = map toJunctionBox coordsId
 where
  toJunctionBox (id, [x, y, z]) = JunctionBox id $ Position (read x) (read y) (read z)
  coordsId = zip [0 ..] coords
  coords = map (splitIf (== ',')) $ lines input

type Circuit = Set.Set ID

newtype Edges = Edges (Heap.Heap Edge)
  deriving (Show)

type ID = Int
data Edge = Edge (ID, ID) Float -- make sure comparison is on Float
  deriving (Show)

-- potential problem: what if different edges have same weight
instance Eq Edge where
  Edge _ dist1 == Edge _ dist2 = dist1 == dist2

instance Ord Edge where
  Edge _ dist1 <= Edge _ dist2 = dist1 <= dist2

data Position = Position Int Int Int
  deriving (Show, Eq)

getX :: Position -> Int
getX (Position val _ _) = val

getY :: Position -> Int
getY (Position _ val _) = val

getZ :: Position -> Int
getZ (Position _ _ val) = val

vectorLength :: Position -> Position -> Float
vectorLength posA posB = sqrt $ xdiff ** 2 + ydiff ** 2 + zdiff ** 2
 where
  xdiff = fromIntegral $ getX posA - getX posB
  ydiff = fromIntegral $ getY posA - getY posB
  zdiff = fromIntegral $ getZ posA - getZ posB

distance :: JunctionBox -> JunctionBox -> Float
distance (JunctionBox _ posA) (JunctionBox _ posB) = vectorLength posA posB

data JunctionBox = JunctionBox ID Position
  deriving (Show, Eq)

getID :: JunctionBox -> ID
getID (JunctionBox id _) = id

possibleEdges :: [JunctionBox] -> Edges
possibleEdges junctionBoxes = Edges $ Heap.fromList nubEdges
 where
  -- this results in a list with duplicate edges next to each other
  edges = [Edge (min (getID jb1) (getID jb2), max (getID jb1) (getID jb2)) (distance jb1 jb2) | jb1 <- junctionBoxes, jb2 <- junctionBoxes, jb1 /= jb2]
  -- so remove every second element (=> removes duplicates)
  nubEdges = map snd $ filter (even . fst) $ zip [0 ..] edges

splitIf :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitIf predicate xs = case dropWhile predicate xs of
  [] -> []
  xs' -> prefix : splitIf predicate xs''
   where
    (prefix, xs'') = break predicate xs'

overlaps :: (Eq a, Ord a) => Set.Set a -> Set.Set a -> Bool
overlaps set = not . null . (set `Set.intersection`)
