{-# LANGUAGE TupleSections #-}

import qualified Data.Array.IArray as IA
import qualified Data.Ix as Ix
import Data.List (elemIndex, partition, transpose)
import qualified Data.Set as Set
import Utils (runSolution)

-------------------
-- Defs and parsers
-------------------

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

-------------------
-- part 1
-------------------

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

getStartPosition :: Grid -> Coord
getStartPosition grid = head $ [coord | (coord, tile) <- IA.assocs grid, tile == Start]

distanceToEveryTile :: Grid -> [(Coord, Int)]
distanceToEveryTile grid = concat [map (,steps) (Set.toList coords) | (steps, coords) <- zip [0 ..] stepsUntilEnd]
  where
    startPos = getStartPosition grid
    stepsUntilEnd = takeWhile (not . Set.null) iterateSteps

    iterateSteps :: [Set.Set Coord]
    iterateSteps = map fst $ iterate next (Set.singleton startPos, Set.empty)
      where
        next :: (Set.Set Coord, Set.Set Coord) -> (Set.Set Coord, Set.Set Coord)
        next (curr, seen) = (newlyReached, seen')
          where
            step' s coord = Set.union s $ step grid coord
            reachable = Set.foldl step' Set.empty curr
            newlyReached = Set.difference reachable seen
            seen' = Set.union seen curr

day21part1 :: [String] -> Int
day21part1 input = length [steps | (_, steps) <- distanceToEveryTile grid, sameParity steps, steps <= stepsTaken]
  where
    stepsTaken = 64
    grid = parseInput input
    sameParity = if odd stepsTaken then odd else even

------------------------------------------
-- something to work with part 2 example
------------------------------------------

stepBoundless :: Grid -> Coord -> Set.Set Coord
stepBoundless grid (x, y) =
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

iterateInfSteps :: Grid -> [Set.Set Coord]
iterateInfSteps grid = iterate nextTurn (Set.singleton startPos)
  where
    startPos = head $ [coord | (coord, tile) <- IA.assocs grid, tile == Start]

    step' :: Set.Set Coord -> Coord -> Set.Set Coord
    step' s coord = Set.union s $ stepBoundless grid coord

    nextTurn :: Set.Set Coord -> Set.Set Coord
    nextTurn = Set.foldl step' Set.empty

day21part2Example :: [String] -> Int
day21part2Example input = Set.size $ iterateInfSteps grid !! 100
  where
    grid = parseInput input

-------------------
-- part 2. 
-- This solution depends on several special properties of puzzle input.
-- Would not work for any arbitary map.
-------------------

day21part2 :: [String] -> Int
day21part2 input = totalArea stepsTaken
  where
    stepsTaken = 26501365

    grid = parseInput input
    startPos = getStartPosition grid
    (_, (xBound, yBound)) = IA.bounds grid
    distanceToBoundary = fst startPos
    mapSize = xBound + 1

    both :: (a -> b) -> (a, a) -> (b, b)
    both f (a, b) = (f a, f b)

    totalArea :: Int -> Int
    totalArea stepsTaken' = (n * n) * (oddCount + evenCount) + n * (evenCornerCount + 2 * oddMiddleCount + oddCornerCount) + oddMiddleCount
      where
        n = (stepsTaken' - distanceToBoundary) `div` mapSize :: Int

        (oddTiles, evenTiles) = partition (odd . snd) $ distanceToEveryTile grid
        (oddCount, evenCount) = both length (oddTiles, evenTiles)

        (oddMiddleCount, oddCornerCount) = both length $ partition ((<= distanceToBoundary) . snd) oddTiles
        (evenMiddleCount, evenCornerCount) = both length $ partition ((<= distanceToBoundary) . snd) evenTiles

main :: IO ()
main = do
  runSolution 21 day21part1
  runSolution 21 day21part2
