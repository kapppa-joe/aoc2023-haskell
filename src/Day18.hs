import Utils (runSolution)
import Data.Char (isHexDigit)

data Direction = R | D | L | U deriving (Ord, Eq, Enum, Show, Read)

type Coord = (Int, Int)

type Perimeter = Int

directions :: [(Int, Int)]
directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

type DigPlan = [(Direction, Int)]

parseDigPlan :: [String] -> (DigPlan, DigPlan)
parseDigPlan input = unzip $ map parseLine input
  where
    parseLine line = case words line of
      [dir, meter, rest] ->
        let planOne = (read dir, read meter) :: (Direction, Int)
            colourHex = filter isHexDigit rest 
            (hex, dirNum) = splitAt 5 colourHex
            planTwo = (toEnum $ read dirNum, read ("0x" ++ hex)) :: (Direction, Int)
         in (planOne, planTwo)
      _ -> error $ "fail to parse line: " ++ line

dig :: DigPlan -> (Perimeter, [Coord])
dig plan = dig' plan (0, 0) 0 []
  where
    dig' :: DigPlan -> Coord -> Perimeter -> [Coord] -> (Perimeter, [Coord])
    dig' [] _ peri points = (peri, points)
    dig' (curr : rest) (x, y) peri points =
      let (dir, n) = curr
          (dx, dy) = directions !! fromEnum dir
          (x1, y1) = (x + dx * n, y + dy * n)
          peri' = peri + n
          points' = (x1, y1) : points
       in dig' rest (x1, y1) peri' points'

calcArea :: [Coord] -> Int
calcArea points = abs $ shoelane (zip points (drop 1 points)) `div` 2
  where
    shoelane [] = 0
    shoelane (curr : rest) =
      let ((x0, y0), (x1, y1)) = curr
          v = x0 * y1 - y0 * x1
       in v + shoelane rest

day18 :: ((DigPlan, DigPlan) -> DigPlan) -> [String] -> Int
day18 fstOrSnd input = perimeter + innerPoints
  where
    plan = fstOrSnd $ parseDigPlan input
    (perimeter, points) = dig plan
    area = calcArea points
    innerPoints = area + 1 - perimeter `div` 2

day18part1 :: [String] -> Int
day18part1 = day18 fst

day18part2 :: [String] -> Int
day18part2 = day18 snd


main = do
  runSolution 18 day18part2