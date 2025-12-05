import Data.Maybe (fromJust, isJust)

main = do
  file <- readFile "input.txt"
  let grid = textToGrid file
  print $ length $ accessiblePaperRolls grid

data Cell = PaperRoll | Nil
  deriving (Show, Eq)

-- ensure that all [Cell] have same length
-- for n x m grid, (0,0) is top left, (n,0) is bottom left
type Grid = [[Cell]]

gridSize :: Grid -> (Int, Int)
gridSize grid = (length grid, length (head grid))

accessiblePaperRolls :: Grid -> [(Int, Int)]
accessiblePaperRolls grid = filter isAccessible [(i, j) | i <- [0 .. height - 1], j <- [0 .. width - 1]]
 where
  isAccessible idx = cellIsPaperRoll grid idx && surroundingRollsCount idx < 4
  surroundingRollsCount idx = length $ surroundingPaperRolls grid idx
  (height, width) = gridSize grid

surroundingPaperRolls :: Grid -> (Int, Int) -> [Cell]
surroundingPaperRolls grid idx = filter (== PaperRoll) $ surroundingCells grid idx

surroundingCells :: Grid -> (Int, Int) -> [Cell]
surroundingCells grid (i, j) = map fromJust $ filter (/= Nothing) surrounding
 where
  surrounding = topRow ++ midRow ++ botRow
  topRow = [cellAt (i - 1, j + 1), cellAt (i, j + 1), cellAt (i + 1, j + 1)]
  midRow = [cellAt (i - 1, j), cellAt (i + 1, j)]
  botRow = [cellAt (i + 1, j - 1), cellAt (i, j - 1), cellAt (i - 1, j - 1)]
  cellAt = getCell grid

textToGrid :: String -> Grid
textToGrid textgrid = map (map charToCell) grid_lines
 where
  grid_lines = lines textgrid
  charToCell c
    | c == '@' = PaperRoll
    | otherwise = Nil

cellIsPaperRoll :: Grid -> (Int, Int) -> Bool
cellIsPaperRoll grid idx = exists && isPaperRoll
 where
  isPaperRoll = cell == Just PaperRoll
  exists = isJust cell
  cell = getCell grid idx

getCell :: Grid -> (Int, Int) -> Maybe Cell
getCell grid (i, j)
  | i >= 0 && i < height && j >= 0 && j < width = Just $ (grid !! i) !! j
  | otherwise = Nothing
 where
  (height, width) = gridSize grid
