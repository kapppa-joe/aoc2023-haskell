import Data.List (elemIndex, intercalate)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Utils (runSolution)

type Coord = (Int, Int)

type RockMap = Map.Map Coord Char

type CycleLength = Int

type BeforeRepeat = Int

data Direction = North | West | South | East

data Platform = Platform (Map.Map Coord Char) Int Int deriving (Eq)

instance Show Platform where
  show (Platform m xBound yBound) =
    let showTile x y = Map.findWithDefault '.' (x, y) m
        joinRows = intercalate "\n"
        tiles = joinRows [[showTile x y | x <- [1 .. xBound]] | y <- [1 .. yBound]]
     in tiles

parsePlatform :: [String] -> Platform
parsePlatform s =
  let xBound = length $ head s
      yBound = length s
      coords = [(x, y) | x <- [1 .. xBound], y <- [1 .. yBound]]
      parsedMap =
        Map.fromList
          [ ((x, y), obj)
            | (x, y) <- coords,
              let obj = s !! (y - 1) !! (x - 1),
              obj /= '.'
          ]
   in Platform parsedMap xBound yBound

slideOneRow :: [Maybe Char] -> [Maybe Char]
slideOneRow [] = []
slideOneRow (Just '#' : xs) = Just '#' : slideOneRow xs
slideOneRow v =
  let (rollingZone, rest) = span (/= Just '#') v
      roundRocks = length [x | x <- rollingZone, x == Just 'O']
      rollResult = replicate (length rollingZone - roundRocks) Nothing ++ replicate roundRocks (Just 'O')
   in rollResult ++ slideOneRow rest

newRockPositionsInRow :: RockMap -> [Coord] -> [(Coord, Char)]
newRockPositionsInRow m coords =
  let rowAfterSlide = slideOneRow [Map.lookup coord m | coord <- coords]
      filterEmpty coord maybeRock = case maybeRock of
        Just rock -> Just (coord, rock)
        Nothing -> Nothing
   in catMaybes $ zipWith filterEmpty coords rowAfterSlide

listCoordsTowards :: Direction -> Platform -> [[Coord]]
listCoordsTowards direction (Platform _ xBound yBound) =
  case direction of
    North -> [[(x, y) | y <- reverse [1 .. yBound]] | x <- [1 .. xBound]]
    South -> [[(x, y) | y <- [1 .. yBound]] | x <- [1 .. xBound]]
    East -> [[(x, y) | x <- [1 .. xBound]] | y <- [1 .. yBound]]
    West -> [[(x, y) | x <- reverse [1 .. xBound]] | y <- [1 .. yBound]]

tilt :: Direction -> Platform -> Platform
tilt direction p@(Platform r x y) =
  let rows = listCoordsTowards direction p
      updatedMapList = concatMap (newRockPositionsInRow r) rows
   in Platform (Map.fromList updatedMapList) x y

totalLoad :: Platform -> Int
totalLoad (Platform m _ yBound) =
  let roundRocksYCoords = [y | ((_, y), obj) <- Map.toList m, obj == 'O']
      loadAt y = yBound - y + 1
   in sum $ map loadAt roundRocksYCoords

day14part1 :: [String] -> Int
day14part1 input =
  let platform = parsePlatform input
      tiltedPlatform = tilt North platform
   in totalLoad tiltedPlatform

spin :: Platform -> Platform
spin p =
  foldl (flip tilt) p [North, West, South, East]

findRepeatCycle :: (Eq a) => [a] -> Maybe (BeforeRepeat, CycleLength)
findRepeatCycle s = findRepeatCycle' s 0
  where
    findRepeatCycle' [] _ = Nothing
    findRepeatCycle' (x : xs) seen =
      case elemIndex x (take seen s) of
        Just n -> Just (n, seen - n)
        Nothing -> findRepeatCycle' xs (seen + 1)

trimRepetition :: Int -> BeforeRepeat -> CycleLength -> Int
trimRepetition n a b = ((n - a) `mod` b) + a

day14part2 :: [String] -> Int
day14part2 input =
  let platform = parsePlatform input
      spinResults = iterate spin platform
      repeatCycle = findRepeatCycle spinResults
   in case repeatCycle of
        Nothing -> error "failed to detect duplication cycle"
        Just (a, b) -> totalLoad $ spinResults !! trimRepetition 1000000000 a b

main :: IO ()
main = do
  runSolution 14 day14part2
