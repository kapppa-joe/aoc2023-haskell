{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (liftM2)
import qualified Data.Array as Ix
import Data.List (uncons)
import Data.List.Split (splitOneOf)
import Data.Maybe (mapMaybe)
import Utils (runSolution, fst', snd', trd)

-------------------
-- Defs and parsers
-------------------

type Coord3D = (Integer, Integer, Integer)

type Velocity3D = (Integer, Integer, Integer)

type Coord2D = (Integer, Integer)

type Velocity2D = (Integer, Integer)

data Axis = X | Y | Z deriving (Eq, Ord, Show, Enum)

data Hailstone = Hail {initPos :: Coord3D, velocity :: Velocity3D} deriving (Eq, Ord, Show)

type Line2D = (Integer, Integer, Integer, Integer)

toTriples :: [Integer] -> [Coord3D]
toTriples xs = case take 3 xs of
  [] -> []
  [x, y, z] -> (x, y, z) : toTriples (drop 3 xs)
  _ -> error "failed to parse coord"

parseHailstones :: [String] -> [Hailstone]
parseHailstones = map parseLine
  where
    parseLine :: String -> Hailstone
    parseLine s = Hail coord vel
      where
        extractedNumbers = map read $ filter (not . null) $ splitOneOf " @," s :: [Integer]
        (coord, vel) = case toTriples extractedNumbers of
          [a, b] -> (a, b)
          _ -> error "failed to parse input"

-------------------
-- Part 1. Just middle school algebra
-- ...But took me > 1 hr to figure out a bug about Int overflow. :<
-------------------

solveEq :: Line2D -> Line2D -> Maybe (Rational, Rational)
solveEq (a, b, c, d) (e, f, g, h)
  | divider == 0 = Nothing
  | otherwise = Just (x, y)
  where
    numeratorX = b * c * g - a * d * g + c * e * h - c * f * g
    numeratorY = a * d * h - b * c * h - d * e * h + d * f * g
    divider = c * h - d * g

    x = fromIntegral numeratorX / fromIntegral divider :: Rational
    y = (-1) * fromIntegral numeratorY / fromIntegral divider :: Rational

bothTrue :: (a -> Bool) -> (a -> Bool) -> a -> Bool
bothTrue = liftM2 (&&)

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (a : rest) = [(a, b) | b <- rest] ++ makePairs rest

extract2DParams :: Axis -> Axis -> Hailstone -> Line2D
extract2DParams axisA axisB hail
  | axisA == axisB = error "wrong input param. should choose two different axes"
  | otherwise = (axisA' hail.initPos, axisB' hail.initPos, axisA' hail.velocity, axisB' hail.velocity)
  where
    getter axis = case axis of
      X -> fst'
      Y -> snd'
      Z -> trd
    (axisA', axisB') = (getter axisA, getter axisB)

extractXY :: Hailstone -> Line2D
extractXY = extract2DParams X Y

solveEq' :: (Hailstone, Hailstone) -> Maybe (Rational, Rational)
solveEq' (hailA, hailB) = solveEq (extractXY hailA) (extractXY hailB)

data Case = Parallel | CrossInPast | OutOfRange | WillCross (Rational, Rational) deriving (Eq, Show)

checkIntersectionCase :: (Integer, Integer) -> Hailstone -> Hailstone -> Case
checkIntersectionCase testRange hailA hailB = case intersection of
  Nothing -> Parallel
  Just (x, y) -> determineCase (x, y)
  where
    intersection = solveEq (extractXY hailA) (extractXY hailB)

    determineCase :: (Rational, Rational) -> Case
    determineCase (x, y)
      | not bothInFuture = CrossInPast
      | not bothInBound = OutOfRange
      | otherwise = WillCross (x, y)
      where
        bothInFuture = inFuture hailA x && inFuture hailB x
        bothInBound = inBound x && inBound y

    sameSign :: Rational -> Integer -> Bool
    sameSign a b = (< 0) a == (< 0) b

    inFuture :: Hailstone -> Rational -> Bool
    inFuture hail x = sameSign xDiff vx
      where
        xDiff = x - fromIntegral (fst' hail.initPos)
        vx = fst' hail.velocity

    inBound :: Rational -> Bool
    inBound = bothTrue (Ix.inRange testRange . floor) (Ix.inRange testRange . ceiling)

countIntersections :: (Integer, Integer) -> [Hailstone] -> Int
countIntersections testRange hailstones = length $ filter willCross [check pair | pair <- allPairs]
  where
    allPairs = makePairs hailstones
    check = uncurry $ checkIntersectionCase testRange

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

-------------------
-- Part 2
-- Assume the thrown rock speed as vRock,
-- if we convert the whole system to the referece frame of the rock by vHail' = vHail - vRock
-- then what observed by the rock would be like "me stand still but every hailstone just magically hit me"
-- Base on this assumption, search for intersection point of every hails in the converted system, which will be then the start pos of rock.
-- Set the search space as [-1000 - 1000] for each x y z. Turn out to be more than enough for the given input.
-------------------

linesOverlap :: Line2D -> Line2D -> Bool
linesOverlap (a, b, c, d) (e, f, g, h) = sameSlope && sameIntercept
  where
    sameSlope = c * h == d * g
    sameIntercept = (b * c - a * d) * g == (f * g - e * h) * c

shiftReferenceFrame2D :: Axis -> Axis -> Velocity2D -> Hailstone -> Line2D
shiftReferenceFrame2D axisA axisB (vx, vy) hail = (x0, y0, dx0 - vx, dy0 - vy)
  where
    (x0, y0, dx0, dy0) = extract2DParams axisA axisB hail

detectIntersect :: [Line2D] -> Maybe Coord2D
detectIntersect shiftedHails = detectIntersect' (tail shiftedHails) Nothing
  where
    firstHail = head shiftedHails

    detectIntersect' :: [Line2D] -> Maybe (Rational, Rational) -> Maybe Coord2D
    detectIntersect' [] (Just (vx, vy)) = Just (floor vx, floor vy)
    detectIntersect' [] Nothing = Nothing
    detectIntersect' (line : rest) knownIntersection
      | sameline = checkNext
      | otherwise = case (knownIntersection, currIntersection) of
          (_, Nothing) -> Nothing
          (Nothing, Just _) -> checkNext'
          (Just a, Just b) -> if a == b then checkNext else Nothing
      where
        sameline = linesOverlap firstHail line
        currIntersection = solveEq firstHail line
        checkNext = detectIntersect' rest knownIntersection
        checkNext' = detectIntersect' rest currIntersection

searchRockVelocity2D :: [Hailstone] -> Axis -> Axis -> [Velocity2D] -> Maybe (Velocity2D, Coord2D)
searchRockVelocity2D hailstones axisA axisB searchRange = case uncons search of
  Nothing -> Nothing
  Just (x, _) -> Just x
  where
    search = mapMaybe verifyRockVelocity searchRange

    verifyRockVelocity :: Velocity2D -> Maybe (Velocity2D, Coord2D)
    verifyRockVelocity (vx, vy) = case detectIntersect shiftedHails of
      Nothing -> Nothing
      Just (x, y) -> Just ((vx, vy), (x, y))
      where
        shiftedHails = map (shiftReferenceFrame2D axisA axisB (vx, vy)) hailstones

day24part2 :: [String] -> (Velocity3D, Integer)
day24part2 input = searchResult
  where
    hailstones = parseHailstones input
    searchRange = [1 .. 1000]

    searchRangeXY = [(a * vx, b * vy) | vx <- searchRange, vy <- searchRange, a <- [1, -1], b <- [1, -1]]
    xySearchResult = searchRockVelocity2D hailstones X Y searchRangeXY

    searchResult :: (Velocity3D, Integer)
    searchResult = case xySearchResult of
      Nothing -> error "no result found in given search space"
      Just ((rockVelX, rockVelY), (x, y)) -> searchXZ
        where
          searchRangeXZ = [(rockVelX, c * vz) | vz <- searchRange, c <- [1, -1]]
          xzSearchResult = searchRockVelocity2D hailstones X Z searchRangeXZ

          searchXZ = case xzSearchResult of
            Nothing -> error "failed to determine Z velocity"
            Just ((_, rockVelZ), (_, z)) -> ((rockVelX, rockVelY, rockVelZ), x + y + z)

main :: IO ()
main = do
  -- testWithExample "24" day24part2
  runSolution 24 day24part2