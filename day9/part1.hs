main = do
  file <- readFile "input.txt"
  let points = parseInput file
  print $ findMaximumRectArea points

parseInput :: String -> [Coords]
parseInput = map (toCoords . splitIf (== ',')) . lines
 where
  toCoords [x, y] = (read x, read y)

findMaximumRectArea :: [Coords] -> Int
findMaximumRectArea corners = maximum $ map area allRects
 where
  allRects = possibleRects corners

possibleRects :: [Coords] -> [Rectangle]
possibleRects coords =
  [makeRect p1 p2 | p1@(x1, y1) <- coords, p2@(x2, y2) <- coords, x1 /= x2 && y1 /= y2]
 where
  makeRect = rectFromOpposingCorners

--   lBot, lTop, rTop, rBot
data Rectangle = Rect Coords Coords Coords Coords
  deriving (Show, Eq)

area :: Rectangle -> Int
area rect = width rect * height rect

width :: Rectangle -> Int
width (Rect (x1, _) _ _ (x2, _)) = x2 - x1 + 1

height :: Rectangle -> Int
height (Rect (_, y1) (_, y2) _ _) = y2 - y1 + 1

type Coords = (Int, Int)

rectFromOpposingCorners :: Coords -> Coords -> Rectangle
rectFromOpposingCorners p1@(x1, y1) p2@(x2, y2)
  | x1 <= x2 && y1 <= y2 = Rect p1 (x1, y2) p2 (x2, y1)
  | otherwise = Rect (x1, y2) p1 (x2, y1) p2

splitIf :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitIf predicate xs = case dropWhile predicate xs of
  [] -> []
  xs' -> prefix : splitIf predicate xs''
   where
    (prefix, xs'') = break predicate xs'
