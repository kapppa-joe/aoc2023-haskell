{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Array.IArray as IA
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap
import Data.Ix (inRange, range)
import Data.List (transpose)
import qualified Data.Set as Set
import Utils (runSolution)

type Coord = (Int, Int)

type HeatLoss = Int

type Grid = IA.Array Coord HeatLoss

type Direction = (Int, Int)

type PrevMove = Direction

data State = State {heatloss :: HeatLoss, coord :: Coord, prevMove :: PrevMove, stepsTaken :: [Coord]} deriving (Eq, Ord, Show)

type State' = (HeatLoss, State)

type Seen = Set.Set (Coord, PrevMove)

type PrioQueue = MinPrioHeap HeatLoss State

parseInput :: [String] -> Grid
parseInput s = IA.listArray ((1, 1), (xBound, yBound)) $ map readChar $ concat $ transpose s
  where
    xBound = length $ head s
    yBound = length s
    readChar c = read [c] :: Int

pop :: PrioQueue -> (State, PrioQueue)
pop queue = case Heap.view queue of
  Nothing -> error "no path to goal"
  Just ((_, state), queue') -> (state, queue')

directions :: [Direction]
directions = [(0, 1), (0, -1), (-1, 0), (1, 0)]

backturn :: Direction -> Direction
backturn (a, b) = ((-1) * a, (-1) * b)

heatlossInMove :: Grid -> Coord -> Coord -> HeatLoss 
-- Only for calculating straight line motions
heatlossInMove grid start end = sum [grid IA.! c | c <- travelled]
  where
    travelled = if start < end then tail $ range (start, end) else init $ range (end, start)

shortestDist :: Grid -> Coord -> Int -> Int -> HeatLoss
shortestDist grid start minMove maxMove = heatloss $ shortestDist' initQueue initSeen
  where
    initQueue = Heap.singleton (0, State 0 start (0, 0) [])
    initSeen = Set.empty
    goal = snd $ IA.bounds grid

    shortestDist' :: PrioQueue -> Seen -> State
    shortestDist' queue seen
      | currState.coord == goal = currState
      | hasSeen currState = shortestDist' queue' seen
      | otherwise = processNext
      where
        (currState, queue') = pop queue

        hasSeen :: State -> Bool
        hasSeen state = Set.member (state.coord, state.prevMove) seen

        processNext =
          let (x0, y0) = currState.coord
              updatedSeen = Set.insert (currState.coord, currState.prevMove) seen
              allowedDirs = [dir | dir <- directions, dir /= currState.prevMove, dir /= backturn currState.prevMove]
              nextMoves =
                [ ((x, y), (dx, dy))
                  | (dx, dy) <- allowedDirs,
                    move <- [minMove .. maxMove],
                    let (x, y) = (x0 + dx * move, y0 + dy * move),
                    inRange (IA.bounds grid) (x, y),
                    Set.notMember ((x, y), (dx, dy)) seen
                ]
              nextStates =
                [ (heatloss', State heatloss' newPos move (newPos : currState.stepsTaken))
                  | (newPos, move) <- nextMoves,
                    let heatloss' = heatlossInMove grid currState.coord newPos + currState.heatloss
                ]
              updatedQueue = foldl (flip Heap.insert) queue' nextStates
           in shortestDist' updatedQueue updatedSeen

day17part1 :: [String] -> HeatLoss
day17part1 input = shortestDist grid (1, 1) 1 3
  where
    grid = parseInput input

day17part2 :: [String] -> HeatLoss
day17part2 input = shortestDist grid (1, 1) 4 10
  where
    grid = parseInput input

main :: IO ()
main = do
  runSolution 17 day17part2
