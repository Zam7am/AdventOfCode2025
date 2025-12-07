import Data.Either (lefts)
import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Debug.Trace (trace)

main = do
  file <- readFile "input.txt"
  let tachyonManifolds = fileToManifold file
  print $ length $ getSplits tachyonManifolds

type TachyonManifolds = Seq.Seq (Int, Seq.Seq Char)

type Coord = (Int, Int)

type Cache = Map.Map Coord (Set.Set Coord)

initCache :: Cache
initCache = Map.empty

-- NOTE: zip zips endlessly for one infinite sequence, despite that working for lists
fileToManifold :: String -> TachyonManifolds
fileToManifold file = Seq.zip (Seq.fromList [0 .. len - 1]) fileLines
 where
  len = length fileLines
  fileLines = Seq.fromList $ map Seq.fromList fileLinesList
  fileLinesList = lines file

getSplits :: TachyonManifolds -> Set.Set Coord
getSplits manifolds = fst $ getSplits' (Seq.drop 1 manifolds) (1, startIdx) initCache
 where
  startIdx = (Seq.length . Seq.takeWhileL (/= 'S')) fstLine
  fstLine = snd . fromJust $ Seq.lookup 0 manifolds

getSplits' :: TachyonManifolds -> Coord -> Cache -> (Set.Set Coord, Cache)
getSplits' Seq.Empty _ cache = (Set.empty, cache)
getSplits' manifolds coord@(_, beamSourceCol) cache
  | reachedEnd = (Set.empty, cache)
  | otherwise = (mergedSplits, Map.insert coord mergedSplits cache)
 where
  mergedSplits = insertSplitter $ leftSubTreeSplits `Set.union` rightSubTreeSplits

  rightSubTreeSplits
    | Map.member rightSource cache = fromJust $ Map.lookup rightSource cache -- no need to recompute
    | otherwise = fst $ getSplits' manifoldRest rightSource cache

  leftSubTreeSplits
    | Map.member leftSource cache = fromJust $ Map.lookup leftSource cache -- no need to recompute
    | otherwise = fst $ getSplits' manifoldRest leftSource cache

  insertSplitter = Set.insert (rowIdx, beamSourceCol)
  (leftSource, rightSource) = ((nextRowIdx, beamSourceCol - 1), (nextRowIdx, beamSourceCol + 1))

  nextRowIdx = rowIdx + 1
  (rowIdx, _) = manifoldDropped `Seq.index` 0

  reachedEnd = null manifoldRest
  manifoldRest = if (not . null) manifoldDropped then Seq.drop 1 manifoldDropped else manifoldDropped
  manifoldDropped = dropUntilSplitFound manifolds beamSourceCol

dropUntilSplitFound :: TachyonManifolds -> Int -> TachyonManifolds
dropUntilSplitFound manifolds beamSourceCol = Seq.dropWhileL (not . splitFound beamSourceCol) manifolds

splitFound :: Int -> (Int, Seq.Seq Char) -> Bool
splitFound colIdx (_, row) =
  case Seq.lookup colIdx row of
    Just char -> char == '^'
    Nothing -> False
