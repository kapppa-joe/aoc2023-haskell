import qualified Data.Array as Ix
import qualified Data.Array.IArray as IA
import Data.List (elemIndex, transpose, maximumBy)
import qualified Data.Set as Set
import Utils (testWithExample, runSolution)
import Data.Function (on)

-------------------
-- Defs and parsers
-------------------

type Coord = (Int, Int)

type Direction = (Int, Int)

data Tile = Path | Forest | SlopeN | SlopeE | SlopeS | SlopeW deriving (Eq, Ord, Enum)

instance Show Tile where
  show tile = [".#^>v<" !! fromEnum tile]

type Grid = IA.Array Coord Tile

directions :: [(Int, Int)]
directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

readTile :: Char -> Tile
readTile c = case c `elemIndex` ".#^>v<" of
  Just g -> toEnum g
  Nothing -> error "failed to parse tile"

parseInput :: [String] -> Grid
parseInput s = IA.listArray ((1, 1), (xBound, yBound)) $ map readTile $ concat $ transpose s
  where
    xBound = length (head s)
    yBound = length s

slopeTiles :: [Tile]
slopeTiles = [SlopeN, SlopeE, SlopeS, SlopeW]

sumVec :: Coord -> Coord -> Coord
sumVec (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)


possibleNexts :: Bool -> Tile -> [Direction]
possibleNexts isSlippery currTile
  | not isSlippery = directions
  | currTile == Path = directions
  | currTile `elem` slopeTiles = [directions !! (fromEnum currTile - 2)]
  | otherwise = error "something went wrong"

day23Part1 :: [String] -> Int
day23Part1 input = dfs (2, 1) Set.empty
  where
    grid = parseInput input
    (_, (xBound, yBound)) = IA.bounds grid
    goal = (xBound - 1, yBound)
    isSlippery = True

    walkable :: Coord -> Bool
    walkable coord = Ix.inRange (IA.bounds grid) coord && grid IA.! coord /= Forest

    dfs :: Coord -> Set.Set Coord -> Int
    dfs curr seen = case nexts of 
      [] -> if curr == goal then Set.size seen else 0
      _ -> maximum possibilities  --maximumBy (compare `on` Set.size) possibilities
      where
        currTile = grid IA.! curr
        possibleNexts' = possibleNexts isSlippery currTile

        nexts =
          [ nextCoord
            | (dx, dy) <- possibleNexts',
              let nextCoord = sumVec curr (dx, dy),
              walkable nextCoord,
              Set.notMember nextCoord seen
          ]
        
        seen' = Set.insert curr seen 
        possibilities = [dfs next seen' | next <- nexts]

main = do
  -- testWithExample "23" day23Part1
  runSolution 23 day23Part1