import qualified Data.Array.IArray as IA
import qualified Data.Ix as Ix
import Data.List (elemIndex, transpose)
import qualified Data.Set as Set
import Utils (runSolution, testWithExample)

type Coord = (Int, Int)

data Tile = Plot | Rock | Start deriving (Eq, Ord, Enum)

instance Show Tile where
  show tile = case tile of
    Plot -> "."
    Rock -> "#"
    Start -> "S"

type Grid = IA.Array Coord Tile

type Direction = (Int, Int)

directions :: [Direction]
directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

readTile :: Char -> Tile
readTile c = case c `elemIndex` ".#S" of
  Just g -> toEnum g
  Nothing -> error "failed to parse tile"

parseInput :: [String] -> Grid
parseInput s = IA.listArray ((1, 1), (xBound, yBound)) $ map readTile $ concat $ transpose s
  where
    xBound = length $ head s
    yBound = length s

step :: Grid -> Coord -> Set.Set Coord
step m (x, y) =
  Set.fromList
    [ (x1, y1)
      | (dx, dy) <- directions,
        let (x1, y1) = (x + dx, y + dy),
        inRange (x1, y1),
        walkable (x1, y1)
    ]
  where
    inRange = Ix.inRange (IA.bounds m)
    walkable coord = m IA.! coord /= Rock

possibilities :: Grid -> Coord -> [Set.Set Coord]
possibilities m startPos = iterate nextTurn (Set.singleton startPos)
  where
    step' :: Set.Set Coord -> Coord -> Set.Set Coord
    step' s coord = Set.union s $ step m coord

    nextTurn :: Set.Set Coord -> Set.Set Coord
    nextTurn = Set.foldl step' Set.empty

possibilities' :: Grid -> [Set.Set Coord]
possibilities' m = possibilities m startPos
  where
    startPos = head $ [coord | (coord, tile) <- IA.assocs m, tile == Start]

day21part1 :: Int -> [String] -> Int
day21part1 steps input = Set.size $ possibilities' grid !! steps
  where
    grid = parseInput input


main :: IO ()
main = do
  runSolution 21 (day21part1 64)
  -- testWithExample "21" 