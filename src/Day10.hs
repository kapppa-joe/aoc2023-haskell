import Data.List (transpose, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TextIO (readFile)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import UtilsM (runWithParser, debug)

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
  return $ Map.fromList tiles

connectedTo :: Tile -> Coord -> [Coord]
connectedTo Ground _ = []
connectedTo tile (x0, y0) = [(x0 + dx, y0 + dy) | (dx, dy) <- deltas]
  where
    deltas = case tile of
      NS -> [(0, -1), (0, 1)]
      EW -> [(1, 0), (-1, 0)]
      NE -> [(0, -1), (1, 0)]
      NW -> [(0, -1), (-1, 0)]
      SW -> [(0, 1), (-1, 0)]
      SE -> [(0, 1), (1, 0)]
      _ -> []

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

allGrounds :: PipeMaze -> [Coord]
allGrounds m = [coord | (coord, tile) <- Map.toList m, tile == Ground]

adjacentTiles :: Coord -> [Coord]
adjacentTiles (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

-- emptySet :: Set.Set Coord
-- emptySet = Set.empty

adjacentGrounds :: PipeMaze -> Coord -> [Coord]
adjacentGrounds m coord = [neighbour |
  neighbour <- adjacentTiles coord,
  let maybeTile = Map.lookup neighbour m,
  maybeTile == Just Ground]

-- note : need to dedup before counting length
allAdjacentGrounds :: PipeMaze -> [Coord] -> [Coord] -> [Coord]
allAdjacentGrounds m seen nexts
  | null nexts = seen
  | otherwise =
    let
      notSeen = flip notElem seen
      nexts' = filter notSeen $ concat [adjacentGrounds m coord | coord <- nexts] -- `debug` ("seen: " ++ show seen ++ ", next:" ++ show nexts)
      seen' = seen ++ nexts
      in
     allAdjacentGrounds m seen' nexts'



day10part2 m = sort $ Set.toList $ Set.fromList $ allAdjacentGrounds m [] [(1, 1)]
-- day10part2 m = Map.size m

main :: IO ()
main = runWithParser parseTiles day10part2 "example/10.txt"