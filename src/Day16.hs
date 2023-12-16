{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Array.IArray as IA
import Data.List (elemIndex, intercalate, transpose)
import qualified Data.Set as Set
import Utils (testWithExample, testWithExample', runSolution)

data Grid = Empty | MirrorNE | MirrorSE | SplitterV | SplitterH deriving (Eq, Ord, Enum)

gridChars :: [Char]
gridChars = "./\\|-"

instance Show Grid where
  show g = [gridChars !! fromEnum g]

data Direction = North | East | South | West deriving (Eq, Ord, Enum, Show)

type Coord = (Int, Int)

type Cavern = IA.Array Coord Grid

type BeamHead = (Coord, Direction)

data BeamPaths = BeamPaths {heads :: [BeamHead], seen :: Set.Set BeamHead, energized :: Set.Set Coord} deriving (Show)

printCavern :: Cavern -> IO ()
printCavern cavern =
  let ((_, _), (xBound, yBound)) = IA.bounds cavern
      showGrid g = gridChars !! fromEnum g
   in putStrLn $ intercalate "\n" [[showGrid $ cavern IA.! (x, y) | x <- [1 .. xBound]] | y <- [1 .. yBound]]

parseGrid :: [String] -> Cavern
parseGrid input = IA.listArray ((1, 1), (xBound, yBound)) $ map readChar $ concat $ transpose input
  where
    xBound = length $ head input
    yBound = length input
    readChar :: Char -> Grid
    readChar c = case c `elemIndex` gridChars of
      Just g -> toEnum g
      Nothing -> error "failed to parse grid"

travel :: Cavern -> BeamHead -> [Coord]
travel c ((x0, y0), dir) =
  case dir of
    North -> [(x0, y) | y <- reverse [1 .. y0 - 1]]
    South -> [(x0, y) | y <- [y0 + 1 .. yBound]]
    East -> [(x, y0) | x <- [x0 + 1 .. xBound]]
    West -> [(x, y0) | x <- reverse [1 .. x0 - 1]]
  where
    (_, (xBound, yBound)) = IA.bounds c

changeDir :: Coord -> Direction -> Grid -> [BeamHead]
changeDir coord dir grid = [(coord, newDir) | newDir <- getNewDirections]
  where
    getNewDirections =
      case grid of
        MirrorNE -> [[East, North, West, South] !! fromEnum dir]
        MirrorSE -> [[West, South, East, North] !! fromEnum dir]
        SplitterV -> if dir `elem` [West, East] then [North, South] else [dir]
        SplitterH -> if dir `elem` [North, South] then [East, West] else [dir]
        _ -> error "something went wrong"

processBeam :: Cavern -> BeamHead -> BeamPaths
processBeam cavern b@(_, dir) =
  let (travelled, stop) = span ((== Empty) . snd) [(coord', cavern IA.! coord') | coord' <- travel cavern b]
   in case (travelled, stop) of
        (t, []) -> BeamPaths [] (Set.singleton b) (Set.fromList $ map fst t)
        (t, (coord'', obstacle) : _) ->
          let newBeamHeads = changeDir coord'' dir obstacle
              energized = Set.fromList (coord'' : map fst t)
           in BeamPaths newBeamHeads (Set.singleton b) energized

processBeamPaths :: Cavern -> BeamPaths -> BeamPaths
processBeamPaths cavern BeamPaths {heads, seen, energized} =
  let results = map (processBeam cavern) heads
      energized' = Set.unions $ energized : [path.energized | path <- results]
      seen' = Set.unions $ seen : [path.seen | path <- results]
      heads' = Set.fromList $ concat [path.heads | path <- results]
      newHeads = Set.toList $ Set.difference heads' seen'
   in BeamPaths newHeads seen' energized'




-- BeamPath [] Set.empty Set.empty

-- day16Part1 :: [String] -> Int
day16Part1 :: [String] -> Int
day16Part1 input =
  let cavern = parseGrid input
      initial = BeamPaths [((0, 1), East)] Set.empty Set.empty
      iter = iterate (processBeamPaths cavern) initial
    in head $ [length step.energized | step <- iter, null step.heads]

main = do
  runSolution 16 day16Part1
  -- testWithExample "16" day16Part1