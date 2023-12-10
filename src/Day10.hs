import Data.List (sort, transpose)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust, catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TextIO (readFile)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import UtilsM (debug, runWithParser)

type Parser = Parsec Void Text

data Tile = NS | EW | NE | NW | SW | SE | Ground | Start deriving (Eq, Ord, Enum, Show)

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

allGrounds :: PipeMaze -> [Coord]
allGrounds m = [coord | (coord, tile) <- Map.toList m, tile == Ground]

adjacentTiles :: Coord -> [Coord]
adjacentTiles (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

eightNeighbours :: Coord -> [Coord]
eightNeighbours (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)]]

adjacentGrounds :: PipeMaze -> Coord -> [Coord]
adjacentGrounds m coord =
  [ neighbour
    | neighbour <- adjacentTiles coord,
      let maybeTile = Map.lookup neighbour m,
      maybeTile == Just Ground
  ]

adjacentPipes :: PipeMaze -> Coord -> [Coord]
adjacentPipes m coord =
  [ neighbour
    | neighbour <- eightNeighbours coord,
      let maybeTile = Map.lookup neighbour m,
      isJust maybeTile && maybeTile /= Just Ground
  ]

-- note : need to dedup before counting length
findConnectedGrounds :: PipeMaze -> [Coord] -> [Coord] -> [Coord]
findConnectedGrounds m seen nexts
  | null nexts = seen
  | otherwise =
      let notSeen = flip notElem seen
          nexts' = filter notSeen $ concat [adjacentGrounds m coord | coord <- nexts] -- `debug` ("seen: " ++ show seen ++ ", next:" ++ show nexts)
          seen' = seen ++ nexts
       in findConnectedGrounds m seen' nexts'

dedup :: (Ord a) => [a] -> [a]
dedup xs = Set.toList $ Set.fromList xs

findConnectedGrounds' :: PipeMaze -> Coord -> Set.Set Coord
findConnectedGrounds' m coord = Set.fromList $ findConnectedGrounds m [] [coord]

allGroundGroups :: PipeMaze -> [Set.Set Coord]
allGroundGroups m = dedup [findConnectedGrounds' m coord | coord <- allGrounds m]

surroundingPipes :: PipeMaze -> Set.Set Coord -> Set.Set Coord
surroundingPipes m grounds = Set.fromList $ concat [adjacentPipes m ground | ground <- Set.toList grounds]

fixStart :: PipeMaze -> (PipeMaze, Coord, Coord)
fixStart m =
  let start@(x0, y0) = head $ [coord | (coord, tile) <- Map.toList m, tile == Start]
      nexts = take 2 [coord | (coord, tile) <- Map.toList m, start `elem` connectedTo tile coord]
      deltas = Set.fromList [(x1 - x0, y1 - y0) | (x1, y1) <- nexts]
      startTile = head $ [tile | (tile, deltaPattern) <- Map.toList connections, Set.fromList deltaPattern == deltas]
      patchedMaze = Map.insert start startTile m
   in (patchedMaze, start, head nexts)

simplifyMaze :: PipeMaze -> Coord -> Coord -> PipeMaze
simplifyMaze m start next =
  let pipeWithAnimal = Set.fromList $ allConnectedPipes m start next
   in Map.filterWithKey (\key _ -> Set.member key pipeWithAnimal) m



countOccurence :: (Ord a) => [a] -> Map.Map a Int
countOccurence [] = Map.empty
countOccurence (key : xs) =
  let m = countOccurence xs
   in case Map.lookup key m of
        Nothing -> Map.insert key 1 m
        Just _ -> Map.insertWith (+) key 1 m

-- countCrossingPipes :: [Tile] -> Int
-- countCrossingPipes [] = 0
-- countCrossingPipes (x:xs) =



-- countCrossingPipes :: PipeMaze -> Coord -> (Int, Int, Int)
-- countCrossingPipes m (x, y) =
--   let
--     pipes = catMaybes [Map.lookup (x', y) m | x' <- [1..x-1]]
--     counter = countOccurence pipes
--     countType tile = Map.findWithDefault 0 tile counter
--     verticalCrossed = countType NS
--     uShapePairs = countType NE - countType NW -- | + L - J + F - 7
--     nShapePairs = countType SE - countType SW
--     in
--       (verticalCrossed, uShapePairs, nShapePairs)



day10part2 m = 
  let
    (patchedMaze, start, next) = fixStart m
    simplifiedMaze = simplifyMaze patchedMaze start next
    allGrounds' = allGrounds m
   in
    [(coord, countCrossingPipes simplifiedMaze coord) | coord <- allGrounds']

-- Map.filterWithKey
-- allConnectedPipes m start next

-- addSurroundingPipes :: PipeMaze -> PipeMaze
-- addSurroundingPipes m =
--   let
--     allCoords = Map.keys m
--     xBound = maximum [x | (x, _) <- allCoords]
--     yBound = maximum [y | (_, y) <- allCoords]
--     nBorder = [(0, y) | y <- [0..yBound + 1]]  :: [Coord]
--     sBorder = [(0, y) | y <- [0..yBound + 1]]  :: [Coord]
--     eBorder = [(0, y) | y <- [0..yBound + 1]]  :: [Coord]
--     wBorder = [(0, y) | y <- [0..yBound + 1]]  :: [Coord]
--   in
--     m

-- day10part2 m =
--   let start = head $ [coord | (coord, tile) <- Map.toList m, tile == Start]
--       next = head [coord | (coord, tile) <- Map.toList m, start `elem` connectedTo tile coord]
--       pipeWithAnimal = Set.fromList $ allConnectedPipes m start next
--       allGroundGroups' = allGroundGroups m
--       allCoords = Map.keys m
--       xBound = maximum [x | (x, _) <- allCoords]
--       yBound = maximum [y | (_, y) <- allCoords]
--       touchingOuterBorder groundGroup = any (\(x, y) -> x == 1 || y == 1 || x == xBound || y == yBound) (Set.toList groundGroup)
--       groundGroupsNotTouchingBorder = filter (not . touchingOuterBorder) allGroundGroups'
--       enclosedGround = [groundGroup | groundGroup <- groundGroupsNotTouchingBorder, surroundingPipes m groundGroup `Set.isSubsetOf` pipeWithAnimal]
--    in m

--  in (surroundingPipes m $ Set.fromList [(4, 3)]) `Set.isSubsetOf` pipeWithAnimal

--  in [enclosedGround]

-- test m =
--   adjacentPipes m (1, 3)

-- day10part2 m = sort $ Set.toList $ Set.fromList $ findConnectedGrounds m [] [(1, 1)]
-- day10part2 m = surroundingPipes m $ Set.fromList [(3, 7), (4, 7)]
-- day10part2 m = allConnectedPipes m (2,2) (2,3)

main :: IO ()
main = runWithParser parseTiles day10part2 "example/10.txt"