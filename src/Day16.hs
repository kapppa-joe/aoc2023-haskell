{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Array.IArray as IA
import Data.List (elemIndex, intercalate, transpose)
import qualified Data.Set as Set
import Utils (runSolution)

data Object = Empty | MirrorNE | MirrorSE | SplitterV | SplitterH deriving (Eq, Ord, Enum)

gridChars :: [Char]
gridChars = "./\\|-"

instance Show Object where
  show g = [gridChars !! fromEnum g]

data Direction = North | East | South | West deriving (Eq, Ord, Enum, Show, IA.Ix)

type Coord = (Int, Int)

type Cavern = IA.Array Coord Object

type Cursor = (Coord, Direction)

data BeamTrace = BeamTrace {cursors :: [Cursor], seen :: Set.Set Cursor, energized :: Set.Set Coord} deriving (Show)

bounds :: Cavern -> Coord
bounds cavern = (xBound, yBound)
  where
    ((_, _), (xBound, yBound)) = IA.bounds cavern

printCavern :: Cavern -> IO ()
printCavern cavern =
  let ((_, _), (xBound, yBound)) = IA.bounds cavern
      showGrid g = gridChars !! fromEnum g
   in putStrLn $ intercalate "\n" [[showGrid $ cavern IA.! (x, y) | x <- [1 .. xBound]] | y <- [1 .. yBound]]

parseCavern :: [String] -> Cavern
parseCavern input = IA.listArray ((1, 1), (xBound, yBound)) $ map readChar $ concat $ transpose input
  where
    xBound = length $ head input
    yBound = length input
    readChar :: Char -> Object
    readChar c = case c `elemIndex` gridChars of
      Just g -> toEnum g
      Nothing -> error "failed to parse grid"

ahead :: Cavern -> Cursor -> [Coord]
ahead c ((x0, y0), dir) =
  case dir of
    North -> [(x0, y) | y <- reverse [1 .. y0 - 1]]
    South -> [(x0, y) | y <- [y0 + 1 .. yBound]]
    East -> [(x, y0) | x <- [x0 + 1 .. xBound]]
    West -> [(x, y0) | x <- reverse [1 .. x0 - 1]]
  where
    (_, (xBound, yBound)) = IA.bounds c

changeDir :: Coord -> Direction -> Object -> [Cursor]
changeDir coord dir obj = [(coord, newDir) | newDir <- getNewDirections]
  where
    getNewDirections =
      case obj of
        MirrorNE -> [[East, North, West, South] !! fromEnum dir]
        MirrorSE -> [[West, South, East, North] !! fromEnum dir]
        SplitterV -> if dir `elem` [West, East] then [North, South] else [dir]
        SplitterH -> if dir `elem` [North, South] then [East, West] else [dir]
        _ -> error $ "something went wrong. params:" ++ show (coord, dir, obj)

tick :: Cavern -> Cursor -> BeamTrace
tick cavern b@(_, dir) =
  let (travelled, stop) = span ((== Empty) . snd) [(coord', cavern IA.! coord') | coord' <- ahead cavern b]
   in case (travelled, stop) of
        (t, []) -> BeamTrace [] (Set.singleton b) (Set.fromList $ map fst t)
        (t, (coord'', obj) : _) ->
          let newCursors = changeDir coord'' dir obj
              energized = Set.fromList (coord'' : map fst t)
           in BeamTrace newCursors (Set.singleton b) energized

part1StartingPoints :: (Int, Int) -> [Cursor]
part1StartingPoints _ = [((0, 1), East)]

part2StartingPoints :: (Int, Int) -> [Cursor]
part2StartingPoints (xBound, yBound) =
  [((0, y), East) | y <- [1 .. yBound]]
    ++ [((x, 0), South) | x <- [1 .. xBound]]
    ++ [((xBound + 1, y), West) | y <- [1 .. yBound]]
    ++ [((x, yBound + 1), North) | x <- [1 .. xBound]]

day16 :: ((Int, Int) -> [Cursor]) -> [String] -> Int
day16 f input = maximum $ [length $ energizedTiles startingPoint | startingPoint <- startingPoints]
  where
    cavern = parseCavern input
    (xBound, yBound) = bounds cavern
    startingPoints = f (xBound, yBound)

    traceOnce :: BeamTrace -> BeamTrace
    traceOnce BeamTrace {cursors, seen, energized} =
      let results = map (tick cavern) cursors
          energized' = Set.unions $ energized : [path.energized | path <- results]
          seen' = Set.unions $ seen : [path.seen | path <- results]
          cursors' = Set.fromList $ concat [path.cursors | path <- results]
          newCursors = Set.toList $ Set.difference cursors' seen'
       in BeamTrace newCursors seen' energized'

    energizedTiles :: Cursor -> Set.Set Coord
    energizedTiles cursor =
      let iter = iterate traceOnce $ BeamTrace [cursor] Set.empty Set.empty
       in energized . head $ dropWhile (not . null . cursors) iter

day16part1 :: [String] -> Int
day16part1 = day16 part1StartingPoints

day16part2 :: [String] -> Int
day16part2 = day16 part2StartingPoints

main :: IO ()
main = do
  runSolution 16 day16part1
