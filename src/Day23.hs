import qualified Data.Array as Ix
import qualified Data.Array.IArray as IA
import Data.List (elemIndex, transpose)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import Utils (runSolution)

type Coord = (Int, Int)

type Direction = (Int, Int)

data Tile = Path | Forest | SlopeN | SlopeE | SlopeS | SlopeW deriving (Eq, Ord, Enum)

instance Show Tile where
  show tile = [".#^>v<" !! fromEnum tile]

type Grid = IA.Array Coord Tile

type HikingGraph = Map.Map Coord [(Coord, Int)]

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

findStartAndGoal :: Grid -> (Coord, Coord)
findStartAndGoal grid = (start, goal)
  where
    (_, (xBound, yBound)) = IA.bounds grid
    start = head [coord | x <- [1 .. xBound], let coord = (x, 1), grid IA.! coord == Path]
    goal = head [coord | x <- [1 .. xBound], let coord = (x, yBound), grid IA.! coord == Path]

buildGraph :: Grid -> Bool -> HikingGraph
buildGraph grid isSlippery = Map.fromList $ buildGraph' [start] Set.empty []
  where
    (start, goal) = findStartAndGoal grid

    buildGraph' :: [Coord] -> Set.Set Coord -> [(Coord, [(Coord, Int)])] -> [(Coord, [(Coord, Int)])]
    buildGraph' [] _ done = done
    buildGraph' nexts seen done = buildGraph' nexts' seen' $ newlyAdded ++ done
      where
        newlyAdded = [(coord, findConnectedNodes coord) | coord <- nexts]
        seen' = Set.union seen (Set.fromList nexts)
        candidates = [connectedNodes | (_, results) <- newlyAdded, (connectedNodes, _) <- results]
        nexts' = Set.toList $ Set.fromList candidates `Set.difference` seen'

    findConnectedNodes :: Coord -> [(Coord, Int)]
    findConnectedNodes currNode = catMaybes connectedNodes
      where
        connectedNodes = [walkUntilBranch neighbour currNode 0 | neighbour <- neighbours currNode]

        inBound :: Coord -> Bool
        inBound = Ix.inRange (IA.bounds grid)

        walkable :: Coord -> Bool
        walkable coord = inBound coord && grid IA.! coord /= Forest

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

longestDistance :: HikingGraph -> Coord -> Coord -> Int
longestDistance graph start goal = dfs start Set.empty 0
  where
    dfs :: Coord -> Set.Set Coord -> Int -> Int
    dfs curr visited walked
      | curr == goal = walked
      | null nexts = 0
      | canSeeTheGoal = justFinishItAlready -- prune useless branches that visited goal-1 but went elsewhere
      | otherwise = maximum possibilities
      where
        nexts = [(node, dist) | (node, dist) <- fromJust $ Map.lookup curr graph, Set.notMember node visited]
        visited' = Set.insert curr visited
        possibilities = [dfs node visited' (walked + dist) | (node, dist) <- nexts]

        canSeeTheGoal = goal `elem` map fst nexts
        justFinishItAlready = dfs goal' visited' (walked + distToGoal)
          where
            (goal', distToGoal) = head $ filter ((== goal) . fst) nexts

day23 :: Slipperiness -> [String] -> Int
day23 condition input = longestDistance graph start goal
  where
    grid = parseInput input
    (start, goal) = findStartAndGoal grid
    graph = buildGraph grid (condition == Slippery)

data Slipperiness = Slippery | NotSoSlippery deriving (Eq)

day23part1 :: [String] -> Int
day23part1 = day23 Slippery

day23part2 :: [String] -> Int
day23part2 = day23 NotSoSlippery

main :: IO ()
main = do
  runSolution 23 day23part1
  runSolution 23 day23part2