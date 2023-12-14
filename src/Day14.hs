import Data.List (intercalate, elemIndex)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Utils (runSolution, testWithExample)

type Coord = (Int, Int)

type RockMap = Map.Map Coord Char

data Direction = North | West | South | East

data Platform = Platform (Map.Map Coord Char) Int Int deriving (Eq)

instance Show Platform where
  show (Platform m xBound yBound) =
    let showTile x y = Map.findWithDefault '.' (x, y) m
        joinRows = intercalate "\n"
        tiles = joinRows [[showTile x y | x <- [1 .. xBound]] | y <- [1 .. yBound]] :: String
     in tiles

parsePlatform :: [String] -> Platform
parsePlatform s =
  let xBound = length $ head s
      yBound = length s
      coords = [(x, y) | x <- [1 .. xBound], y <- [1 .. yBound]]
      parsedMap = Map.fromList [((x, y), obj) | (x, y) <- coords, let obj = s !! (y - 1) !! (x - 1), obj /= '.']
   in Platform parsedMap xBound yBound

rockSlideSingleRow :: [Maybe Char] -> [Maybe Char]
rockSlideSingleRow [] = []
rockSlideSingleRow (Just '#' : xs) = Just '#' : rockSlideSingleRow xs
rockSlideSingleRow v =
  let (rolling, rest) = span (/= Just '#') v
      countRoundRocks = length [x | x <- rolling, x == Just 'O']
      rolled = replicate (length rolling - countRoundRocks) Nothing ++ replicate countRoundRocks (Just 'O')
   in rolled ++ rockSlideSingleRow rest

newRockPositionsInRow :: RockMap -> [Coord] -> [(Coord, Char)]
newRockPositionsInRow m coords =
  let row = [Map.lookup coord m | coord <- coords] :: [Maybe Char]
      afterSlide = rockSlideSingleRow row
      filterEmpty coord maybeRock = case maybeRock of
        Just rock -> Just (coord, rock)
        Nothing -> Nothing
   in catMaybes $ zipWith filterEmpty coords afterSlide

listCoordsTowards :: Direction -> Platform -> [[Coord]]
listCoordsTowards direction (Platform _ xBound yBound) =
  case direction of
    North -> [[(x, y) | y <- reverse [1 .. yBound]] | x <- [1 .. xBound]]
    South -> [[(x, y) | y <- [1 .. yBound]] | x <- [1 .. xBound]]
    East -> [[(x, y) | x <- [1 .. xBound]] | y <- [1 .. yBound]]
    West -> [[(x, y) | x <- reverse [1 .. xBound]] | y <- [1 .. yBound]]

listCoordsTowardsNorth :: Platform -> [[Coord]]
listCoordsTowardsNorth (Platform _ xBound yBound) =
  [[(x, y) | y <- reverse [1 .. yBound]] | x <- [1 .. xBound]]

tilt :: Direction -> Platform -> Platform
tilt direction p@(Platform r x y) =
  let coordLists = listCoordsTowards direction p
      updatedMapList = concatMap (newRockPositionsInRow r) coordLists
   in Platform (Map.fromList updatedMapList) x y

totalLoad :: Platform -> Int
totalLoad (Platform m _ yBound) =
  let roundRocksYCoords = [y | ((x, y), obj) <- Map.toList m, obj == 'O']
      load y = yBound - y + 1
   in sum $ map load roundRocksYCoords

day14part1 :: [String] -> Int
day14part1 input =
  let p = parsePlatform input
      tiltedPlatform = tilt North p
   in totalLoad tiltedPlatform

spin :: Platform -> Platform
spin p =
  foldl (flip tilt) p [North, West, South, East]

findDuplicateCycle :: Eq a => [a] -> Maybe (Int, Int)
findDuplicateCycle s = findDuplicate' s 0
  where
    findDuplicate' [] _ = Nothing
    findDuplicate' (x:xs) seen =
      case elemIndex x (take seen s) of
        Just n -> Just (n, seen - n)
        Nothing -> findDuplicate' xs (seen + 1)

findSameCycle :: Int -> Int -> Int -> Int
findSameCycle n beforeRepeat repeatCycle =
  ((n - beforeRepeat) `mod` repeatCycle) + beforeRepeat

day14part2 :: [String] -> Int
day14part2 input =
  let
    platform = parsePlatform input
    spinResults = iterate spin platform
    dupCycle = findDuplicateCycle spinResults
  in case dupCycle of
    Nothing -> error "failed to detect duplication cycle"
    Just (a, b) -> totalLoad $ spinResults !! findSameCycle 1000000000 a b 




trial input =
  let p = parsePlatform input
  --  in findDuplicateCycle $ iterate spin p
  in findSameCycle 1000000000 3 7 

--  in take 30 $ zip (map totalLoad (iterate spin p)) [1..]

main = do
  runSolution 14 day14part2
  -- testWithExample "14" day14part2