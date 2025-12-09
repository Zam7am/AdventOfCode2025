import Data.Heap (nub)
import Data.Heap qualified as Heap
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Ord qualified as Ord
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Debug.Trace (trace)

main = do
  file <- readFile "input.txt"
  let junctionBoxes = parseInput file
  let edges = possibleEdges junctionBoxes
  let pairs = shortestEdges edges
  let (_, (Edge (i, j) _) : _, _) = insertUntilFullyConnected (Set.empty, pairs, junctionBoxes)
  print $ product $ map (getX . getPos) $ filter (\jb -> getID jb `elem` [i, j]) junctionBoxes

insertUntilFullyConnected :: (Set.Set Circuit, [Edge], [JunctionBox]) -> (Set.Set Circuit, [Edge], [JunctionBox])
insertUntilFullyConnected (circuits, e : es, jbs)
  | isFullyConnected = (newCircuits, e : es, jbs)
  | otherwise = insertUntilFullyConnected (newCircuits, es, jbs)
 where
  isFullyConnected = fullyConnected jbs newCircuits
  newCircuits = insertIntoCircuit e circuits

fullyConnected :: [JunctionBox] -> Set.Set Circuit -> Bool
fullyConnected jbs circuits = allIds `elem` circuits
 where
  allIds = Set.fromList $ map getID jbs

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
mergeOverlapping circuits = merged `Set.union` removedMerged
 where
  merged = Set.fromList $ map (foldr Set.union Set.empty) pairs
  removedMerged = circuits `Set.difference` Set.fromList (concat pairs)
  pairs = [[c1, c2] | c1 <- cs, c2 <- cs, c1 /= c2, c1 `overlaps` c2] -- merges only two together, however that is enough
  cs = Set.elems circuits

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

getPos :: JunctionBox -> Position
getPos (JunctionBox _ pos) = pos

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
