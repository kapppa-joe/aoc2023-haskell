import Data.List (transpose)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import qualified Data.Set as Set
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import UtilsM (debug, runWithParser, Parser)


data Tile = NS | EW | NE | NW | SW | SE | Ground | Start deriving (Eq, Ord, Enum)

instance Show Tile where
  show tile = case tile of 
    NS -> "|"
    EW -> "-"
    NE -> "L"
    NW -> "J"
    SW -> "7"
    SE -> "F"
    Ground -> "."
    Start -> "S"

type Coord = (Int, Int)

type PipeMaze = Map.Map Coord Tile

parseTile :: Parser (Coord, Tile)
parseTile = do
  tile <-
    choice
      [ NS <$ char '|',
        EW <$ char '-',
        NE <$ char 'L',
        NW <$ char 'J',
        SW <$ char '7',
        SE <$ char 'F',
        Ground <$ char '.',
        Start <$ char 'S'
      ]
  SourcePos _ line col <- getSourcePos
  let x = unPos col - 1
      y = unPos line
  return ((x, y), tile)

parseTiles :: Parser PipeMaze
parseTiles = do
  tiles <- sepBy parseTile $ optional eol
  eof
  return $ Map.fromList tiles

connections :: Map.Map Tile [(Int, Int)]
connections =
  Map.fromList
    [ (NS, [(0, -1), (0, 1)]),
      (EW, [(1, 0), (-1, 0)]),
      (NE, [(0, -1), (1, 0)]),
      (NW, [(0, -1), (-1, 0)]),
      (SW, [(0, 1), (-1, 0)]),
      (SE, [(0, 1), (1, 0)])
    ]

connectedTo :: Tile -> Coord -> [Coord]
connectedTo Ground _ = []
connectedTo tile (x0, y0) = [(x0 + dx, y0 + dy) | (dx, dy) <- deltas]
  where
    deltas = fromMaybe [] (Map.lookup tile connections)

moveAlongPipe :: PipeMaze -> (Coord, Coord) -> (Coord, Coord)
moveAlongPipe m (prev, curr) =
  let currTile = fromJust $ Map.lookup curr m
      next = head [coord | coord <- connectedTo currTile curr, coord /= prev]
   in (curr, next)

day10part1 :: Map.Map Coord Tile -> Int
day10part1 m =
  let startLocation = head $ [coord | (coord, tile) <- Map.toList m, tile == Start]
      nexts = take 2 [coord | (coord, tile) <- Map.toList m, startLocation `elem` connectedTo tile coord]
      moveAlongPipe' = moveAlongPipe m
      iterator = transpose [iterate moveAlongPipe' (startLocation, next) | next <- nexts]
      allTheSame xs = all (== head xs) $ tail xs
      endCondition coords = allTheSame [curr | (_, curr) <- coords]
   in length (takeWhile (not . endCondition) iterator) + 1

allConnectedPipes :: PipeMaze -> Coord -> Coord -> [Coord]
allConnectedPipes m start next =
  let iterator = iterate (moveAlongPipe m) (start, next)
      endCondition (_, curr) = curr == start
   in start : map snd (takeWhile (not . endCondition) iterator)

patchStartingTile :: PipeMaze -> (PipeMaze, Coord, Coord)
patchStartingTile m =
  let start@(x0, y0) = head $ [coord | (coord, tile) <- Map.toList m, tile == Start]
      nexts = take 2 [coord | (coord, tile) <- Map.toList m, start `elem` connectedTo tile coord]
      deltas = Set.fromList [(x1 - x0, y1 - y0) | (x1, y1) <- nexts]
      startTile = head $ [tile | (tile, deltaPattern) <- Map.toList connections, Set.fromList deltaPattern == deltas]
      patchedMaze = Map.insert start startTile m
   in (patchedMaze, start, head nexts)

extractMainMaze :: PipeMaze -> Coord -> Coord -> PipeMaze
extractMainMaze m start next =
  let pipeWithAnimal = Set.fromList $ allConnectedPipes m start next
   in Map.filterWithKey (\key _ -> Set.member key pipeWithAnimal) m

countIntersections :: [Tile] -> Int
countIntersections [] = 0
countIntersections [c]
  | c == NS = 1
  | otherwise = 0
countIntersections (a:b:cs)
  | a == NE && b == SW = 1 + countIntersections cs
  | a == SE && b == NW = 1 + countIntersections cs
  | a == NS = 1 + countIntersections (b:cs)
  | otherwise = countIntersections (b:cs)


day10part2 :: Map.Map (Int, Int) Tile -> Int
day10part2 m =
  let
    (patchedMaze, start, next) = patchStartingTile m
    simplifiedMaze = extractMainMaze patchedMaze start next
    xBound = maximum [x | (x, _) <- Map.keys m]
    yBound = maximum [y | (_, y) <- Map.keys m]
    allGroundTiles = [(x, y) | x <- [1..xBound], y <- [1..yBound], Map.notMember (x, y) simplifiedMaze]
    allPipesAtWestSideOf (x, y) = relevantPipes $ catMaybes [Map.lookup (x', y) simplifiedMaze | x' <- [1..x-1]]
    relevantPipes = filter (\tile -> tile `elem` [NS, NE, NW, SW, SE])
    isInner = odd . countIntersections . allPipesAtWestSideOf
   in
    length $ filter isInner allGroundTiles

main :: IO ()
main = runWithParser parseTiles day10part2 "puzzle/10.txt"