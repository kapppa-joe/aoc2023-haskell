{-# LANGUAGE TupleSections #-}

import Control.Monad (liftM2)
import qualified Data.Array.IArray as IA
import qualified Data.Ix as Ix
import Data.List (elemIndex, partition, transpose)
import qualified Data.Map as Map
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

directions :: [(Int, Int)]
directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

readTile :: Char -> Tile
readTile c = case c `elemIndex` ".#S" of
  Just g -> toEnum g
  Nothing -> error "failed to parse tile"

parseInput :: [String] -> Grid
parseInput s = IA.listArray ((0, 0), (xBound, yBound)) $ map readTile $ concat $ transpose s
  where
    xBound = length (head s) - 1
    yBound = length s - 1

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

data Direction = N | E | S | W deriving (Eq, Ord, Show, Enum)

step'' :: Grid -> Coord -> Set.Set Coord
step'' grid (x, y) =
  Set.fromList
    [ (x1, y1)
      | (dx, dy) <- directions,
        let (x1, y1) = (x + dx, y + dy),
        walkable (x1, y1)
    ]
  where
    translate (x', y') = (x' `mod` mapSize, y' `mod` mapSize)
    walkable coord = grid IA.! translate coord /= Rock
    mapSize = (+ 1) . snd . snd $ IA.bounds grid

stepsForInfGrid :: Grid -> [Set.Set Coord]
stepsForInfGrid m = iterate nextTurn (Set.singleton startPos)
  where
    startPos = head $ [coord | (coord, tile) <- IA.assocs m, tile == Start]

    step' :: Set.Set Coord -> Coord -> Set.Set Coord
    step' s coord = Set.union s $ step'' m coord

    nextTurn :: Set.Set Coord -> Set.Set Coord
    nextTurn = Set.foldl step' Set.empty

day21part2Example :: [String] -> Int
day21part2Example input = Set.size $ stepsForInfGrid grid !! 100
  where
    grid = parseInput input

day21part2 :: [String] -> Int
day21part2 input = calcTotalArea 26501365
  where
    grid = parseInput input
    initStartPos = head $ [coord | (coord, tile) <- IA.assocs grid, tile == Start]
    (_, (xBound, yBound)) = IA.bounds grid
    distToBoundary = fst initStartPos
    mapSize = xBound + 1

    both :: (a -> b) -> (a, a) -> (b, b)
    both f (a, b) = (f a, f b)

    (oddTiles, evenTiles) = partition (odd . snd) distanceToEveryTile
    (oddCount, evenCount) = both length (oddTiles, evenTiles)

    (oddMiddleCount, oddCornerCount) = both length $ partition ((<= distToBoundary) . snd) oddTiles
    (evenMiddleCount, evenCornerCount) = both length $ partition ((<= distToBoundary) . snd) evenTiles

    calcTotalArea :: Int -> Int
    calcTotalArea totalSteps = totalArea
      where
        n = (totalSteps - distToBoundary) `div` mapSize :: Int
        totalArea = (n * n) * (oddCount + evenCount) + n * (evenCornerCount + 2 * oddMiddleCount + oddCornerCount) + oddMiddleCount

    stepsUntilEnd = takeWhile (not . Set.null) $ iterateStepsFrom initStartPos

    distanceToEveryTile :: [(Coord, Int)]
    distanceToEveryTile = concat [map (,steps) (Set.toList coords) | (steps, coords) <- zip [0 ..] stepsUntilEnd]
    -- length $ [steps | ((x, y), steps) <- stepsToReachEveryTile, even steps]

    -- entryPoints :: [(Direction, Coord)]
    -- entryPoints = map (uncurry mapEntryPoint) exitPoints
    --   where
    --     mapEntryPoint N (x, _y) = (S, (x, yBound))
    --     mapEntryPoint S (x, _y) = (N, (x, 0))
    --     mapEntryPoint E (_x, y) = (W, (0, y))
    --     mapEntryPoint W (_x, y) = (E, (xBound, y))

    -- exitPoints = [(dir, coord) | (dir, (_step, coord)) <- Map.toList exitPointsWithSteps]

    -- reachBoundary :: Direction -> Coord -> Bool
    -- reachBoundary N = (== 0) . snd
    -- reachBoundary E = (== xBound) . fst
    -- reachBoundary S = (== yBound) . snd
    -- reachBoundary W = (== 0) . fst

    -- exitPointsWithSteps :: Map.Map Direction (Int, Coord)
    -- exitPointsWithSteps = Map.fromList [(dir, bfs initStartPos f) | dir <- enumFrom N, let f = reachBoundary dir]

    -- distance :: Coord -> Coord -> Int
    -- distance startPos goalpos = stepsToReach startPos (== goalpos)

    -- stepsToReach :: Coord -> (Coord -> Bool) -> Int
    -- stepsToReach = (fst .) . bfs

    -- bfs :: Coord -> (Coord -> Bool) -> (Int, Coord)
    -- bfs startPos endCondition = (steps, goal)
    --   where
    --     goal = case filter endCondition (Set.toList reached) of
    --       [x] -> x
    --       _ -> error "more than one coord meet the given condition at the same time"
    --     (steps, reached) = head iter
    --     iter = dropWhile (not . endCondition') $ zip [0 ..] (iterateStepsFrom startPos)
    --     endCondition' (_step, coords) = any endCondition $ Set.toList coords

    iterateStepsFrom :: Coord -> [Set.Set Coord]
    iterateStepsFrom startPos = map fst $ iterate nextSteps (Set.singleton startPos, Set.empty)
      where
        nextSteps :: (Set.Set Coord, Set.Set Coord) -> (Set.Set Coord, Set.Set Coord)
        nextSteps (curr, seen) = (newlyReached, seen')
          where
            step' s coord = Set.union s $ step grid coord
            reachable = Set.foldl step' Set.empty curr
            newlyReached = Set.difference reachable seen
            seen' = Set.union seen curr

-- takeUntilRepeat :: (Eq a) => [a] -> [a]
-- takeUntilRepeat xs = map fst taken
--   where
--     pairs = zip xs (drop 2 xs)
--     taken = takeWhile (not . pairMatch) pairs
--     pairMatch (a, b) = a == b

-- trial2 input = zip [0 ..] $ map length $ takeUntilRepeat iter
--   where
--     grid = parseInput input
--     iter = possibilities' grid

-- trial3 input = zip [0 ..] (map Set.size iter) !! 50 :: (Int, Int)
--   where
--     grid = parseInput input
--     iter = possibilities' grid

-- trial4 input = take 199 (zip [0 ..] $ map Set.size $ stepsForInfGrid grid)
--   where
--     grid = parseInput input

main :: IO ()
main = do
  -- testWithExample "21" day21part2
  -- testWithExample "21" day21part2Example
  runSolution 21 day21part2
