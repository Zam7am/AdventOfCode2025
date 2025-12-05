import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Sequence qualified as S

main = do
  file <- readFile "input.txt"
  let grid = textToGrid file
  let cleanGrid = removePaperRolls grid
  print $ gridDiff grid cleanGrid

removePaperRolls :: Grid -> Grid
removePaperRolls grid
  | null accessible = updatedGrid
  | otherwise = removePaperRolls updatedGrid
 where
  updatedGrid = foldl cleanCell grid accessible
  accessible = accessiblePaperRolls grid

accessiblePaperRolls :: Grid -> [(Int, Int)]
accessiblePaperRolls grid = filter isAccessible indices
 where
  isAccessible idx = cellIsPaperRoll grid idx && surroundingRollsCount idx < 4
  surroundingRollsCount idx = length $ surroundingPaperRolls grid idx
  indices = [(i, j) | i <- [0 .. height - 1], j <- [0 .. width - 1]]
  (height, width) = gridSize grid

surroundingPaperRolls :: Grid -> (Int, Int) -> [Cell]
surroundingPaperRolls grid idx = filter (== PaperRoll) $ surroundingCells grid idx

surroundingCells :: Grid -> (Int, Int) -> [Cell]
surroundingCells grid (i, j) = catMaybes surrounding
 where
  surrounding = topRow ++ midRow ++ botRow
  topRow = [cellAt (i - 1, j + 1), cellAt (i, j + 1), cellAt (i + 1, j + 1)]
  midRow = [cellAt (i - 1, j), cellAt (i + 1, j)]
  botRow = [cellAt (i + 1, j - 1), cellAt (i, j - 1), cellAt (i - 1, j - 1)]
  cellAt = getCell grid

-- ensure that all [Cell] have same length
-- for n x m grid, (0,0) is top left, (n,0) is bottom left
newtype Grid = Grid (S.Seq (S.Seq Cell))
  deriving (Show, Eq)

data Cell = PaperRoll | Nil
  deriving (Show, Eq)

textToGrid :: String -> Grid
textToGrid textgrid = listsToGrid $ map (map charToCell) grid_lines
 where
  grid_lines = lines textgrid
  charToCell c
    | c == '@' = PaperRoll
    | otherwise = Nil

listsToGrid :: [[Cell]] -> Grid
listsToGrid listGrid = Grid grid
 where
  grid = S.fromList rows
  rows = map S.fromList listGrid

gridSize :: Grid -> (Int, Int)
gridSize (Grid rows) = (length rows, length (rows `S.index` 0))

cellIsPaperRoll :: Grid -> (Int, Int) -> Bool
cellIsPaperRoll grid idx = cell == Just PaperRoll
 where
  cell = getCell grid idx

getCell :: Grid -> (Int, Int) -> Maybe Cell
getCell (Grid rows) (i, j) = case row of
  Just row -> S.lookup j row
  Nothing -> Nothing
 where
  row = S.lookup i rows

-- assumes valid cell
cleanCell :: Grid -> (Int, Int) -> Grid
cleanCell (Grid rows) (i, j) = Grid updatedRows
 where
  updatedRows = S.update i updatedRow rows
  updatedRow = S.update j Nil row
  row = rows `S.index` i

-- compute the number of different cells
gridDiff :: Grid -> Grid -> Int
gridDiff (Grid rowsA) (Grid rowsB) = sum $ fmap rowDiff (S.zip rowsA rowsB)
 where
  rowDiff (rowA, rowB) = sum (S.zipWith incrementIfNotEqual rowA rowB)
  incrementIfNotEqual cellA cellB
    | cellA /= cellB = 1
    | otherwise = 0
