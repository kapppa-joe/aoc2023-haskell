{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Array as Ix
import qualified Data.Array.IArray as IA
import Data.List (elemIndex, transpose)
import qualified Data.Set as Set
import Utils (testWithExample, debug, runSolution)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as Map
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap


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

data State = State {distance :: Int, currNode :: Coord, visited :: Set.Set Coord } deriving (Eq, Ord, Show)
type PrioQueue = MinPrioHeap Int State


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

allowedDirections :: Bool -> Tile -> [Direction]
allowedDirections isSlippery currTile
  | not isSlippery = directions
  | currTile == Path = directions
  | currTile `elem` slopeTiles = [directions !! (fromEnum currTile - 2)]
  | otherwise = error "something went wrong"

sumVec :: Coord -> Direction -> Coord
sumVec (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

inBound :: Grid -> Coord -> Bool
inBound grid = Ix.inRange (IA.bounds grid)

type HikingGraph = Map.Map Coord [(Coord, Int)]


buildGraph :: Grid -> Bool -> HikingGraph
buildGraph grid isSlippery = Map.fromList $ buildGraph' [start] Set.empty []

  where
    (_, (xBound, yBound)) = IA.bounds grid
    start = (2, 1)
    goal = (xBound - 1, yBound)


    buildGraph' :: [Coord] -> Set.Set Coord -> [(Coord, [(Coord, Int)])] -> [(Coord, [(Coord, Int)])]
    buildGraph' [] _ done = done
    buildGraph' nexts seen done = buildGraph' nexts' seen' $ newlyAdded ++ done
      where
        newlyAdded = [(coord, findNextNodes coord) | coord <- nexts]
        seen' = Set.union seen (Set.fromList nexts)
        candidates = [connectedNodes | (_, results) <- newlyAdded, (connectedNodes, _) <- results]
        nexts' = Set.toList $ Set.fromList candidates `Set.difference` seen'


    findNextNodes :: Coord ->  [(Coord, Int)]
    findNextNodes currNode = catMaybes connectedNodes
      where
        startTile = grid IA.! currNode

        connectedNodes = [walkUntilBranch neighbour currNode 0 | neighbour <- neighbours currNode]

        walkable :: Coord -> Bool
        walkable coord = inBound grid coord && grid IA.! coord /= Forest

        neighbours :: Coord -> [Coord]
        neighbours curr =
          [ next
            | (dx, dy) <- allowedDirections isSlippery (grid IA.! curr),
              let next = sumVec curr (dx, dy),
              walkable next
          ]

        walkUntilBranch :: Coord -> Coord -> Int -> Maybe (Coord, Int)
        walkUntilBranch curr prev' steps = case nexts of
          [next] -> walkUntilBranch next curr (steps + 1)
          [] -> if curr == goal then Just (curr, steps + 1) else Nothing
          _ -> Just (curr, steps + 1)
          where
            currTile = grid IA.! curr
            nexts = filter (/= prev') $ neighbours curr


-- pop :: PrioQueue -> (State, PrioQueue)
-- pop queue = case Heap.view queue of
--   Nothing -> error "no path to goal"
--   Just ((_, state), queue') -> (state, queue')


-- longestDistance :: HikingGraph -> Coord -> Coord -> Int
-- longestDistance graph start goal = longestDistance' initQueue Map.empty
--   where
--     initQueue = Heap.singleton (0, State 0 start Set.empty) :: PrioQueue

--     longestDistance' :: PrioQueue -> Map.Map Coord Int -> Int
--     longestDistance' queue seen
--       | Heap.isEmpty queue = (-1) * fromJust (Map.lookup goal seen)
--       | False `debug` (show $ Heap.viewHead queue) = error ""
--       | state.distance < knownDist = longestDistance' updatedQueue seen'
--       -- | Map.member state.currNode seen = longestDistance' queue' seen
--       -- | otherwise = longestDistance' queue' seen
--       | otherwise = longestDistance' updatedQueue seen
--         where
--           (state, queue') = pop queue
--           nexts = Map.findWithDefault [] state.currNode graph
--           queueEntries = [(newDist, State newDist coord visited') |
--                (coord, dist') <- nexts,
--                Set.notMember coord state.visited,
--                let newDist = (-1) * dist' + state.distance,
--                let visited' = Set.insert state.currNode state.visited
--             ]
--           updatedQueue = foldl (flip Heap.insert) queue' queueEntries
--           knownDist = Map.findWithDefault 1 state.currNode seen
--           seen' = Map.insert state.currNode state.distance seen


longestDistance :: HikingGraph -> Coord -> Coord -> Int
longestDistance graph start goal = longestDistance' start Set.empty 0
  where
    longestDistance' :: Coord -> Set.Set Coord -> Int -> Int
    longestDistance' curr visited walked
      | curr == goal = walked
      | null nexts = 0
      | goal `elem` map fst nexts = justDoItAlready
      | otherwise = maximum possibilities
        where
          nexts = [(node, dist) |(node, dist) <- fromJust $ Map.lookup curr graph, Set.notMember node visited]
          visited' = Set.insert curr visited
          possibilities = [longestDistance' node visited' (walked + dist) | (node, dist) <- nexts]
          justDoItAlready = longestDistance' goal' visited' (walked + distToGoal)
            where
              (goal', distToGoal) = head $ filter ((==goal) . fst) nexts 




  -- where
  --   shortestDistance' curr seen 

-- 
try input = longestDistance graph start goal
  where
    grid = parseInput input
    (_, (xBound, yBound)) = IA.bounds grid
    start = (2,1)
    goal = (xBound - 1, yBound)
    isSlippery = False
    graph = buildGraph grid isSlippery


main = do
  -- testWithExample "23" try
  runSolution 23 try


-- walkUntilBranch :: HikingTrail -> Coord -> Set.Set Coord -> (Coord, Coord, Int)
-- walkUntilBranch trail start blocked = walkUntilBranch' start start 0
--   where
--     walkUntilBranch' :: Coord -> Coord -> Int -> (Coord, Coord, Int)
--     walkUntilBranch' curr prev steps = case nexts of
--       [] -> (curr, prev, steps)
--       [next] -> walkUntilBranch' next curr (steps + 1)
--       _ -> (curr, prev, steps)
--       where
--         nexts = [next | next <- trail IA.! curr, next /= prev, Set.notMember next blocked]

toHikingTrail :: Bool -> Grid -> HikingTrail
toHikingTrail isSlippery grid = IA.array (IA.bounds grid) trail
  where
    trail = [(coord, walkableNeighbours (coord, tile)) | (coord, tile) <- walkableTiles]

    walkableTiles :: [(Coord, Tile)]
    walkableTiles = filter ((/= Forest) . snd) $ IA.assocs grid

    walkable :: Coord -> Bool
    walkable coord = Ix.inRange (IA.bounds grid) coord && grid IA.! coord /= Forest

    neighbours' = allowedDirections isSlippery

    walkableNeighbours :: (Coord, Tile) -> [Coord]
    walkableNeighbours ((x, y), currTile) =
      [ (x1, y1)
        | (dx, dy) <- neighbours' currTile,
          let (x1, y1) = (x + dx, y + dy),
          walkable (x1, y1)
      ]

-- walkUntilBranch :: HikingTrail -> Coord -> Set.Set Coord -> (Coord, Coord, Int)
-- walkUntilBranch trail start blocked = walkUntilBranch' start start 0
--   where
--     walkUntilBranch' :: Coord -> Coord -> Int -> (Coord, Coord, Int)
--     walkUntilBranch' curr prev steps = case nexts of
--       [] -> (curr, prev, steps)
--       [next] -> walkUntilBranch' next curr (steps + 1)
--       _ -> (curr, prev, steps)
--       where
--         nexts = [next | next <- trail IA.! curr, next /= prev, Set.notMember next blocked]

-- day23part1 :: [String] -> Int
-- day23part1 input = dfs (2, 1) Set.empty 0
--   where
--     isSlippery = False
--     grid = parseInput input
--     trail = toHikingTrail isSlippery grid
--     (_, (xBound, yBound)) = IA.bounds grid
--     goal = (xBound - 1, yBound)

--     walkUntilBranch' = walkUntilBranch trail

--     dfs :: Coord -> Set.Set Coord -> Int -> Int
--     dfs curr blocked totalWalked
--       -- \| False `debug` (show curr ++ show blocked ++ show totalWalked) = 1000
--       | curr == goal = totalWalked
--       | null nexts = 0
--       | length nexts == 1 = dfsFromNextBranch
--       | otherwise = maximum possibilities
--       where
--         nexts = [next | next <- trail IA.! curr, Set.notMember next blocked]
--         blocked' = Set.insert curr blocked
--         branches = [walkUntilBranch' next blocked' | next <- nexts]
--         tilesBeforeBranch = [prev | (arrivedAt, prev, _) <- branches, arrivedAt /= goal]

--         dfsFromNextBranch = dfs branchHead updatedBlocked (totalWalked + walked)
--           where
--             updatedBlocked = Set.insert prev blocked'
--             (branchHead, prev, walked) = walkUntilBranch' curr blocked

--         possibilities =
--           [ dfs branchHead updatedBlocked (totalWalked + walked + 1)
--             | let updatedBlocked =
--                     Set.union blocked' (Set.fromList tilesBeforeBranch),
--               (branchHead, _, walked) <- branches
--           ]

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
-- try input = walkUntilBranch trail (2, 1) Set.empty
--   where
--     trail = toHikingTrail True $ parseInput input

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

-- main = do
--   -- testWithExample "23" day23part1
--   runSolution 23 day23part1