import Data.List (intercalate, transpose)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import qualified Data.Set as Set
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import UtilsM (Parser, runWithParser)

-------------------
-- Defs and parsers
-------------------

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

type PipeMap = Map.Map Coord Tile

data PipeMaze = PipeMaze {pipeMap :: PipeMap, xBound :: Int, yBound :: Int}

instance Show PipeMaze where
  show (PipeMaze m xBound_ yBound_) =
    let showTile x y = show $ Map.findWithDefault Ground (x, y) m
        joinRows = intercalate "\n"
        tiles = joinRows [concat [showTile x y | x <- [1 .. xBound_]] | y <- [1 .. yBound_]]
     in tiles

show' :: PipeMap -> String
show' m =
  let xBound_ = maximum [x | (x, _) <- Map.keys m]
      yBound_ = maximum [y | (_, y) <- Map.keys m]
      maze = PipeMaze m xBound_ yBound_
   in show maze

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

parsePipeMaze :: Parser PipeMaze
parsePipeMaze = do
  tiles <- sepBy parseTile $ optional eol
  eof
  let pipeMap_ = Map.fromList tiles
      xBound_ = maximum [x | ((x, _), _) <- tiles]
      yBound_ = maximum [y | ((_, y), _) <- tiles]
  return $ PipeMaze pipeMap_ xBound_ yBound_

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

---------------
-- Part 1
---------------

connectedTo :: Tile -> Coord -> [Coord]
connectedTo Ground _ = []
connectedTo tile (x0, y0) = [(x0 + dx, y0 + dy) | (dx, dy) <- deltas]
  where
    deltas = fromMaybe [] (Map.lookup tile connections)

moveAlongPipe :: PipeMap -> (Coord, Coord) -> (Coord, Coord)
moveAlongPipe m (prev, curr) =
  let currTile = fromJust $ Map.lookup curr m
      next = head [coord | coord <- connectedTo currTile curr, coord /= prev]
   in (curr, next)

day10part1 :: PipeMaze -> Int
day10part1 (PipeMaze m _ _) =
  let startLocation = head $ [coord | (coord, tile) <- Map.toList m, tile == Start]
      twoWays = take 2 [coord | (coord, tile) <- Map.toList m, startLocation `elem` connectedTo tile coord]
      moveAlongPipe' = moveAlongPipe m
      iterator = transpose [iterate moveAlongPipe' (startLocation, next) | next <- twoWays]
      allTheSame xs = all (== head xs) $ tail xs
      endCondition coords = allTheSame [curr | (_, curr) <- coords] -- two trips arrive at same tile
   in length (takeWhile (not . endCondition) iterator) + 1

---------------
-- Part 2
---------------

allConnectedPipes :: PipeMap -> Coord -> Coord -> [Coord]
allConnectedPipes m start next =
  let iterator = iterate (moveAlongPipe m) (start, next)
      endCondition (_, curr) = curr == start
   in start : map snd (takeWhile (not . endCondition) iterator)

patchStartingTile :: PipeMap -> (PipeMap, Coord, Coord)
patchStartingTile m =
  let startCoord@(x0, y0) = head $ [coord | (coord, tile) <- Map.toList m, tile == Start]
      nexts = take 2 [coord | (coord, tile) <- Map.toList m, startCoord `elem` connectedTo tile coord]
      deltas = Set.fromList [(x1 - x0, y1 - y0) | (x1, y1) <- nexts]
      startTile = head $ [tile | (tile, deltaPattern) <- Map.toList connections, Set.fromList deltaPattern == deltas]
      patchedMaze = Map.insert startCoord startTile m
   in (patchedMaze, startCoord, head nexts)

extractMainMaze :: PipeMap -> Coord -> Coord -> PipeMap
extractMainMaze m start next =
  let mainPipe = Set.fromList $ allConnectedPipes m start next
   in Map.filterWithKey (\key _ -> Set.member key mainPipe) m

countVerticalWalls :: [Tile] -> Int
countVerticalWalls [] = 0
countVerticalWalls [tile]
  | tile == NS = 1
  | otherwise = 0
countVerticalWalls (a : b : rest)
  | a == NE && b == SW = 1 + countVerticalWalls rest -- L7 merge to |
  | a == SE && b == NW = 1 + countVerticalWalls rest -- FJ merge to |
  | a == NS = 1 + countVerticalWalls (b : rest)
  | otherwise = countVerticalWalls (b : rest)

day10part2 :: PipeMaze -> Int
day10part2 (PipeMaze m xBound_ yBound_) =
  let (patchedMaze, start, next) = patchStartingTile m
      simplifiedMaze = extractMainMaze patchedMaze start next
      allGroundTiles = [(x, y) | x <- [1 .. xBound_], y <- [1 .. yBound_], Map.notMember (x, y) simplifiedMaze]
      filterVerticalPipes = filter (\tile -> tile `elem` [NS, NE, NW, SW, SE])
      allVerticalPipesAtLeftOf (x, y) = filterVerticalPipes $ catMaybes [Map.lookup (x', y) simplifiedMaze | x' <- [1 .. x - 1]]
      isInner = odd . countVerticalWalls . allVerticalPipesAtLeftOf
   in length $ filter isInner allGroundTiles

main :: IO ()
main = runWithParser parsePipeMaze day10part2 "puzzle/10.txt"