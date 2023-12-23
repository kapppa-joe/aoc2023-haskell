import Control.Monad (liftM2)
import qualified Data.Array as Ix
import qualified Data.Array.IArray as IA
import Data.Function (on)
import Data.List (elemIndex, maximumBy, transpose)
import qualified Data.Set as Set
import Utils (runSolution, testWithExample, debug)

-------------------
-- Defs and parsers
-------------------

type Coord = (Int, Int)

type Direction = (Int, Int)

data Tile = Path | Forest | SlopeN | SlopeE | SlopeS | SlopeW deriving (Eq, Ord, Enum)

instance Show Tile where
  show tile = [".#^>v<" !! fromEnum tile]

type Grid = IA.Array Coord Tile

type HikingTrail = IA.Array Coord [Coord]

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

neighbours :: Bool -> Tile -> [Direction]
neighbours isSlippery currTile
  | not isSlippery = directions
  | currTile == Path = directions
  | currTile `elem` slopeTiles = [directions !! (fromEnum currTile - 2)]
  | otherwise = error "something went wrong"

toHikingTrail :: Bool -> Grid -> HikingTrail
toHikingTrail isSlippery grid = IA.array (IA.bounds grid) trail
  where
    trail = [(coord, walkableNeighbours (coord, tile)) | (coord, tile) <- walkableTiles]

    walkableTiles :: [(Coord, Tile)]
    walkableTiles = filter ((/= Forest) . snd) $ IA.assocs grid

    walkable :: Coord -> Bool
    walkable coord = Ix.inRange (IA.bounds grid) coord && grid IA.! coord /= Forest

    neighbours' = neighbours isSlippery

    walkableNeighbours :: (Coord, Tile) -> [Coord]
    walkableNeighbours ((x, y), currTile) =
      [ (x1, y1)
        | (dx, dy) <- neighbours' currTile,
          let (x1, y1) = (x + dx, y + dy),
          walkable (x1, y1)
      ]

walkUntilBranch :: HikingTrail -> Coord -> Set.Set Coord -> (Coord, Coord, Int)
walkUntilBranch trail start blocked = walkUntilBranch' start start 0
  where
    walkUntilBranch' :: Coord -> Coord -> Int -> (Coord, Coord, Int)
    walkUntilBranch' curr prev steps = case nexts of
      [] -> (curr, prev, steps)
      [next] -> walkUntilBranch' next curr (steps + 1)
      _ -> (curr, prev, steps)
      where
        nexts = [next | next <- trail IA.! curr, next /= prev, Set.notMember next blocked]

day23part1 :: [String] -> Int
day23part1 input = dfs (2,1) Set.empty 0
  where
    isSlippery = False
    grid = parseInput input
    trail = toHikingTrail isSlippery grid
    (_, (xBound, yBound)) = IA.bounds grid
    goal = (xBound - 1, yBound)

    walkUntilBranch' = walkUntilBranch trail

    dfs :: Coord -> Set.Set Coord -> Int -> Int
    dfs curr blocked totalWalked
      -- | False `debug` (show curr ++ show blocked ++ show totalWalked) = 1000
      | curr == goal = totalWalked
      | null nexts = 0
      | length nexts == 1 = dfsFromNextBranch
      | otherwise = maximum possibilities
      where
        nexts = [next | next <- trail IA.! curr, Set.notMember next blocked]
        blocked' = Set.insert curr blocked
        branches = [walkUntilBranch' next blocked' | next <- nexts]
        tilesBeforeBranch = [prev | (arrivedAt, prev, _) <- branches, arrivedAt /= goal]

        dfsFromNextBranch = dfs branchHead updatedBlocked (totalWalked + walked)
          where 
            updatedBlocked = Set.insert prev blocked'
            (branchHead, prev, walked) = walkUntilBranch' curr blocked

        possibilities =
          [dfs branchHead updatedBlocked (totalWalked + walked + 1) |
             let updatedBlocked
                   = Set.union blocked' (Set.fromList tilesBeforeBranch),
             (branchHead, _, walked) <- branches]

-- (arrivedAt, walked') = walkUntilBranch' start blocked
-- nexts = filter trail IA.! arrivedAt

-- case nexts of
--   [] -> if curr == goal then Set.size walked else 0
--   _ -> maximum possibilities -- maximumBy (compare `on` Set.size) possibilities
-- currTile = grid IA.! curr
-- possibleNexts' = possibleNexts isSlippery currTile

-- nexts =
--   [ nextCoord
--     | (dx, dy) <- possibleNexts',
--       let nextCoord = sumVec curr (dx, dy),
--       walkable nextCoord,
--       Set.notMember nextCoord seen
--   ]

-- seen' = Set.insert curr seen
-- possibilities = [dfs next seen' | next <- nexts]

-- try :: [String] -> (Coord, Set.Set Coord)
-- try input = trail IA.! (22, 11)
try input = walkUntilBranch trail (2, 1) Set.empty
  where
    trail = toHikingTrail True $ parseInput input

-- sumVec :: Coord -> Coord -> Coord
-- sumVec (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- possibleNexts :: Bool -> Tile -> [Direction]
-- possibleNexts isSlippery currTile
--   | not isSlippery = directions
--   | currTile == Path = directions
--   | currTile `elem` slopeTiles = [directions !! (fromEnum currTile - 2)]
--   | otherwise = error "something went wrong"

-- walkUntilBranch :: Grid -> Coord -> Set.Set Coord -> (Coord, Set.Set Coord)
-- walkUntilBranch grid curr blocked = (curr, blocked)

-- -- day23Part1 :: [String] -> Int
-- day23Part1 input = findDeadEnds
--   where
--     grid = parseInput input
--     (_, (xBound, yBound)) = IA.bounds grid
--     goal = (xBound - 1, yBound)
--     isSlippery = True

--     walkable :: Coord -> Bool
--     walkable coord = Ix.inRange (IA.bounds grid) coord && grid IA.! coord /= Forest

--     isDeadEnd :: Coord -> Bool
--     isDeadEnd (x, y) = length walkableNeighbour == 1
--       where
--         walkableNeighbour = [(x1, y1) | (dx, dy) <- directions, let (x1, y1) = (x + dx, y + dy), walkable (x1, y1)]

--     dfs :: Coord -> Set.Set Coord -> Int
--     dfs curr seen = case nexts of
--       [] -> if curr == goal then Set.size seen else 0
--       _ -> maximum possibilities -- maximumBy (compare `on` Set.size) possibilities
--       where
--         currTile = grid IA.! curr
--         possibleNexts' = possibleNexts isSlippery currTile

--         nexts =
--           [ nextCoord
--             | (dx, dy) <- possibleNexts',
--               let nextCoord = sumVec curr (dx, dy),
--               walkable nextCoord,
--               Set.notMember nextCoord seen
--           ]

--         seen' = Set.insert curr seen
--         possibilities = [dfs next seen' | next <- nexts]

main = do
  -- testWithExample "23" day23part1
  runSolution 23 day23part1