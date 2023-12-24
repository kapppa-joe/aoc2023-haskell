{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (liftM2)
import qualified Data.Array as Ix
import Data.List.Split (splitOn, splitOneOf)
import Utils (runSolution, testWithExample, debug)
import Data.Maybe (catMaybes)

type X = Integer

type Y = Integer

type Z = Integer

type Coord = (X, Y, Z)

type Velocity = (X, Y, Z)

data Hailstone = Hail {initPos :: Coord, velocity :: Velocity} deriving (Eq, Ord, Show)

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

trd :: (a, b, c) -> c
trd (_, _, z) = z

toTriples :: [Integer] -> [Coord]
toTriples xs = case take 3 xs of
  [] -> []
  [x, y, z] -> (x, y, z) : toTriples (drop 3 xs)
  _ -> error "failed to parse coord"

parseHailstones :: [String] -> [Hailstone]
parseHailstones = map parseLine
  where
    parseLine :: String -> Hailstone
    parseLine s = Hail coord v
      where
        extractedNumbers = map read $ filter (not . null) $ splitOneOf " @," s :: [Integer]
        (coord, v) = case toTriples extractedNumbers of
          [x, y] -> (x, y)
          _ -> error "failed to parse input"

type FourParams = (Integer, Integer, Integer, Integer)

solveEq :: FourParams -> FourParams -> Maybe (Double, Double)
solveEq (a, b, c, d) (e, f, g, h) = trySolveEquation
  where
    trySolveEquation = if divider == 0 then Nothing else Just (x, y)
    numeratorX = b * c * g - a * d * g + c * e * h - c * f * g
    numeratorY = a * d * h - b * c * h - d * e * h + d * f * g
    divider = c * h - d * g
    printout = show numeratorX ++ ", " ++ show numeratorY ++ show divider

    x = fromIntegral numeratorX / fromIntegral divider :: Double
    y = (-1) * fromIntegral numeratorY / fromIntegral divider :: Double

bothTrue :: (a -> Bool) -> (a -> Bool) -> a -> Bool
bothTrue = liftM2 (&&)

data Case = Parallel | CrossInPast | OutOfRange | WillCross (Double, Double) deriving (Eq, Show)

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (a : rest) = [(a, b) | b <- rest] ++ makePairs rest

extractParams :: Hailstone -> FourParams
extractParams hail = (fst' hail.initPos, snd' hail.initPos, fst' hail.velocity, snd' hail.velocity)

solveEq' :: (Hailstone, Hailstone) -> Maybe (Double, Double)
solveEq' (hailA, hailB) = solveEq (extractParams hailA) (extractParams hailB)

checkIntersection :: (Integer, Integer) -> Hailstone -> Hailstone -> Case
checkIntersection testRange hailA hailB = checkResult
  where
    intersection = solveEq (extractParams hailA) (extractParams hailB)

    checkResult = case intersection of
      Nothing -> Parallel
      Just (x, y) -> verify (x, y)

    sameSign :: Double -> Integer -> Bool
    sameSign a b = (< 0) a == (< 0) b

    inFuture :: Hailstone -> Double -> Bool
    inFuture hail x = sameSign xDiff vx
      where
        xDiff = x - fromIntegral (fst' hail.initPos)
        vx = fst' hail.velocity

    inBound :: Double -> Bool
    inBound = bothTrue (Ix.inRange testRange . floor) (Ix.inRange testRange . ceiling)

    verify :: (Double, Double) -> Case
    verify (x, y)
      | not bothInFuture = CrossInPast
      | not bothInBound = OutOfRange
      | otherwise = WillCross (x, y)
      where
        bothInFuture = inFuture hailA x && inFuture hailB x
        bothInBound = inBound x && inBound y


countIntersections :: (Integer, Integer) -> [Hailstone] -> Int
countIntersections testRange hailstones = length $ filter willCross [check pair | pair <- allPairs]
  where
    allPairs = makePairs hailstones
    check = uncurry $ checkIntersection testRange

    willCross :: Case -> Bool
    willCross result = case result of 
      WillCross _ -> True 
      _ -> False

day24part1 :: [String] -> Int
day24part1 input = countIntersections testRange hailstones
  where
    hailstones = parseHailstones input
    testRange = (200000000000000, 400000000000000)
    -- testRange = (7, 27)


linesOverlap :: FourParams -> FourParams -> Bool
linesOverlap (a,b,c,d) (e,f,g,h) = sameSlope && sameIntercept
  where
    sameSlope = c * h == d * g
    sameIntercept = (b * c - a * d) * g   == (f * g -  e * h) * c


shiftReferenceFrame :: (Int, Int) -> Hailstone -> FourParams
shiftReferenceFrame (vx, vy) hail = (x0, y0, dx0 - fromIntegral vx, dy0 - fromIntegral vy)
  where
    (x0, y0, dx0, dy0) = extractParams hail


-- trial :: [String] -> [Maybe (Double, Double)]
-- trial input = map solveEq' allPairs
--   where
--     hailstones = parseHailstones input
-- --     testRange = (200000000000000, 400000000000000)
--     allPairs = makePairs hailstones
-- --     check = uncurry $ checkIntersection testRange
-- --     (a, b, c, d) = (219051609191782, 68260434807407, 146, 364)
--     (e,f,g,h) = (455400538938496, 167482380286201, -109, 219)


main = do
  -- testWithExample "24b" trial
  runSolution 24 day24part1