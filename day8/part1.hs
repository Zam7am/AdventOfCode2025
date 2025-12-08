import Data.Either (fromRight)
import Data.Function (on)
import Data.List as List
import Data.Maybe (fromJust)
import Data.Ord
import Data.Sequence as Seq
import Data.Set qualified as Set
import Data.Text as Text (pack, splitOn)
import Data.Text.Read qualified as R
import Debug.Trace (trace)

main = do
  file <- readFile "test.txt"
  let junctionBoxes = parseInput file
  let connectedJunctionBoxes = connect junctionBoxes
  print $ Seq.sortOn ((\(Closest _ dist) -> dist) . snd) $ fmap (\j -> (fst j, getClosest (snd j))) connectedJunctionBoxes
  print $ connectPairs 10 connectedJunctionBoxes

-- print $ multThreeLargestCircuits $ connect 10 circuits

connectPairs :: Int -> Boxes -> Edges
connectPairs 0 _ = Set.empty
connectPairs _ Seq.Empty = Set.empty
connectPairs n (jb :<| jbs) = Set.insert (otherId, id) $ Set.insert (id, otherId) $ connectPairs (n - 1) jbs
 where
  id = fst jb
  (JunctionBox _ _ _ (Just (Closest otherId _))) = snd jb

type ID = Int
data Closest = Closest ID Float
  deriving (Show, Eq)

data JunctionBox = JunctionBox Int Int Int (Maybe Closest)
  deriving (Show, Eq)

type Boxes = Seq (ID, JunctionBox)
type Edges = Set.Set (Int, Int)

getClosest :: JunctionBox -> Closest
getClosest (JunctionBox _ _ _ closest) = fromJust closest

parseInput :: String -> Boxes
parseInput string = fromList $ List.zip [0 ..] $ map toJunctionBox splitCoords
 where
  toJunctionBox [x, y, z] = JunctionBox (readInt x) (readInt y) (readInt z) Nothing
  splitCoords = map (splitOn delimiter) text
  text = map pack inputLines
  delimiter = pack ","
  inputLines = lines string
  readInt x =
    case R.decimal x of
      Right (val, _) -> val
      Left _ -> error "invalid number"

connect :: Boxes -> Boxes
connect jbs = fmap (closestDistance jbs) jbs

closestDistance :: Boxes -> (ID, JunctionBox) -> (ID, JunctionBox)
closestDistance boxes (id, junctionbox) = updatedJunctionbox
 where
  (updatedJunctionbox, _) = connectJunctionBoxes (id, junctionbox) closestJb
  closestJb = minimumBy (compare `on` (getDistance . snd)) jbs
  jbs = Seq.filter (\j -> snd j /= junctionbox) boxes
  getDistance = distance junctionbox

connectJunctionBoxes :: (ID, JunctionBox) -> (ID, JunctionBox) -> ((ID, JunctionBox), (ID, JunctionBox))
connectJunctionBoxes (id1, jb1) (id2, jb2) = (leftJb, rightJb)
 where
  rightJb = (id2, setClosest jb2 id1 dist)
  leftJb = (id1, setClosest jb1 id2 dist)
  dist = distance jb1 jb2

setClosest :: JunctionBox -> ID -> Float -> JunctionBox
setClosest (JunctionBox x y z maybeClosest) id dist = JunctionBox x y z (Just (Closest id dist))

distance :: JunctionBox -> JunctionBox -> Float
distance (JunctionBox x1 y1 z1 _) (JunctionBox x2 y2 z2 _) = sqrt $ xdiff ** 2 + ydiff ** 2 + zdiff ** 2
 where
  xdiff = fromIntegral $ x1 - x2
  ydiff = fromIntegral $ y1 - y2
  zdiff = fromIntegral $ z1 - z2
