import Data.Maybe (fromJust)
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

type Time = Int

type Distance = Int

type Race = (Time, Distance)

type InitSpeed = Int

parseRace :: Parser [Race]
parseRace = do
  _ <- string "Time:"
  spaces
  times <- sepBy1 (many1 digit) $ many1 $ char ' '
  _ <- endOfLine
  _ <- string "Distance:"
  spaces
  distances <- sepBy1 (many1 digit) $ many1 $ char ' '
  let races = zip (parseInts times) (parseInts distances)
  return races
  where
    parseInts = map read

---------------
-- Part 1
---------------

binarySearch :: (Int -> Bool) -> Int -> Int -> Maybe Int
binarySearch f left right
  | left >= right = if f left then Just left else Nothing
  | f middle = binarySearch f left middle
  | otherwise = binarySearch f (middle + 1) right
  where
    middle = (left + right) `div` 2

canBreakRecord :: Race -> InitSpeed -> Bool
canBreakRecord (time, dist) v =
  (time - v) * v > dist

countWaysToWin :: Race -> Int
countWaysToWin race@(time, _) =
  let middle = time `div` 2
      f = canBreakRecord race
      leftBound = fromJust $ binarySearch f 0 middle
      rightBound = fromJust $ binarySearch (not . f) middle time
   in rightBound - leftBound

------
-- Alternative solution (O1)
-- `(time - v) * v > dist`  can be rewritten as `v*2 - time * v + dist < 0`,
-- from which the bounds of v can be found by quadratic formula
-- as it is an inequality, so if both roots are integers than there are two less integral solutions
------

isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

solveByQuadratic :: Time -> Distance -> Int
solveByQuadratic time dist =
  let a = 1 :: Double
      b = fromIntegral $ (-1) * time :: Double
      c = fromIntegral dist :: Double
      discriminant = (b ** 2 - 4 * a * c)
      rootA = (sqrt discriminant - b) / 2
      rootB = ((-1) * sqrt discriminant - b) / 2
      rightBound = if isInteger rootA then floor rootA - 1 else floor rootA
      leftBound = if isInteger rootB then ceiling rootB + 1 else ceiling rootB
   in rightBound - leftBound + 1

day06part1 :: [Race] -> Int
day06part1 races = product [countWaysToWin race | race <- races]

day06part1' :: [Race] -> Int
day06part1' races = product [solveByQuadratic time dist | (time, dist) <- races]

---------------
-- Part 2
---------------

combineRaces :: [Race] -> Race
combineRaces races =
  let (times, distances) = unzip races
      joinNumbers xs = read $ foldl1 (++) $ map show xs
   in (joinNumbers times, joinNumbers distances)

day06part2 :: [Race] -> Int
day06part2 races = countWaysToWin $ combineRaces races

day06part2' :: [Race] -> Int
day06part2' races = solveByQuadratic time distance
  where
    (time, distance) = combineRaces races

main :: IO ()
main = runWithParser parseRace day06part2 "puzzle/06.txt"
