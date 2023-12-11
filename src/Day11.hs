import Data.List (transpose)
import qualified Data.Set as Set
import Utils (runSolution, runSolutionWithFile, testWithExample)

findEmptyRows :: [String] -> [Int]
findEmptyRows rows = [i + 1 | i <- [0 .. length rows - 1], all (== '.') $ rows !! i]

findEmptyColumns :: [String] -> [Int]
findEmptyColumns = findEmptyRows . transpose

type Coord = (Int, Int)

type CosmicImage = Set.Set Coord

parseCosmicImage :: [String] -> CosmicImage
parseCosmicImage rows =
  let xBound = length (head rows) - 1
      yBound = length rows - 1
      extractedPoints =
        [ (x + 1, y + 1)
          | x <- [0 .. xBound],
            y <- [0 .. yBound],
            rows !! y !! x == '#'
        ]
   in Set.fromList extractedPoints

expandSpace :: Int -> [Int] -> [Int] -> CosmicImage -> CosmicImage
expandSpace expandFactor xs ys image =
  let expandX xNum = Set.map (\(x, y) -> (if x > xNum then x + expandFactor else x, y))
      expandY yNum = Set.map (\(x, y) -> (x, if y > yNum then y + expandFactor else y))
      compose = foldl1 (.)
      expandAll = compose (map expandY ys ++ map expandX xs)
   in expandAll image

parseImageAndExpand :: Int -> [String] -> CosmicImage
parseImageAndExpand expandFactor rows =
  let rawImage = parseCosmicImage rows
      ys = findEmptyRows rows
      xs = findEmptyColumns rows
   in expandSpace expandFactor xs ys rawImage

makePairs :: Set.Set Coord -> [(Coord, Coord)]
makePairs set =
  let makePairs' [] = []
      makePairs' (a : rest) = [(a, b) | b <- rest] ++ makePairs' rest
   in makePairs' $ Set.toList set

runDay11WithExpandFactor :: Int -> [String] -> Int
runDay11WithExpandFactor expandFactor rows =
  let image = parseImageAndExpand expandFactor rows
      everyPair = makePairs image
      distance ((x0, y0), (x1, y1)) = abs (x1 - x0) + abs (y1 - y0)
   in sum [distance pair | pair <- everyPair]

day11part1 :: [String] -> Int
day11part1 = runDay11WithExpandFactor 1

day11part2 :: [String] -> Int
day11part2 = runDay11WithExpandFactor (1000000 - 1)

main :: IO ()
main = runSolution 11 day11part2
