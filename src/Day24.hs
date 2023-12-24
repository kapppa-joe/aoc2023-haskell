{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (liftM2)
import qualified Data.Array as Ix
import Data.List.Split (splitOn, splitOneOf)
import Utils (runSolution, testWithExample, debug)
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)
import Data.List (uncons)

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

type Line2D = (Integer, Integer, Integer, Integer)

solveEq :: Line2D -> Line2D -> Maybe (Rational, Rational)
solveEq (a, b, c, d) (e, f, g, h) = trySolveEquation
  where
    trySolveEquation = if divider == 0 then Nothing else Just (x, y)
    numeratorX = b * c * g - a * d * g + c * e * h - c * f * g
    numeratorY = a * d * h - b * c * h - d * e * h + d * f * g
    divider = c * h - d * g
    printout = show numeratorX ++ ", " ++ show numeratorY ++ show divider

    x = fromIntegral numeratorX / fromIntegral divider :: Rational
    y = (-1) * fromIntegral numeratorY / fromIntegral divider :: Rational

bothTrue :: (a -> Bool) -> (a -> Bool) -> a -> Bool
bothTrue = liftM2 (&&)

data Case = Parallel | CrossInPast | OutOfRange | WillCross (Rational, Rational) deriving (Eq, Show)

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (a : rest) = [(a, b) | b <- rest] ++ makePairs rest

extractParams :: Hailstone -> Line2D
extractParams hail = (fst' hail.initPos, snd' hail.initPos, fst' hail.velocity, snd' hail.velocity)

solveEq' :: (Hailstone, Hailstone) -> Maybe (Rational, Rational)
solveEq' (hailA, hailB) = solveEq (extractParams hailA) (extractParams hailB)

checkIntersection :: (Integer, Integer) -> Hailstone -> Hailstone -> Case
checkIntersection testRange hailA hailB = checkResult
  where
    intersection = solveEq (extractParams hailA) (extractParams hailB)

    checkResult = case intersection of
      Nothing -> Parallel
      Just (x, y) -> verify (x, y)

    sameSign :: Rational -> Integer -> Bool
    sameSign a b = (< 0) a == (< 0) b

    inFuture :: Hailstone -> Rational -> Bool
    inFuture hail x = sameSign xDiff vx
      where
        xDiff = x - fromIntegral (fst' hail.initPos)
        vx = fst' hail.velocity

    inBound :: Rational -> Bool
    inBound = bothTrue (Ix.inRange testRange . floor) (Ix.inRange testRange . ceiling)

    verify :: (Rational, Rational) -> Case
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


linesOverlap :: Line2D -> Line2D -> Bool
linesOverlap (a,b,c,d) (e,f,g,h) = sameSlope && sameIntercept
  where
    sameSlope = c * h == d * g
    sameIntercept = (b * c - a * d) * g   == (f * g -  e * h) * c


shiftReferenceFrame2D :: (Int, Int) -> Hailstone -> Line2D
shiftReferenceFrame2D (vx, vy) hail = (x0, y0, dx0 - fromIntegral vx, dy0 - fromIntegral vy)
  where
    (x0, y0, dx0, dy0) = extractParams hail

verifyIntersect :: [Line2D] -> Maybe (Int, Int)
verifyIntersect shiftedHails = verifyIntersect' (tail shiftedHails) Nothing
  where
    firstHail = head shiftedHails

    verifyIntersect' :: [Line2D] -> Maybe (Rational, Rational) -> Maybe (Int, Int)
    verifyIntersect' [] (Just (vx, vy)) = Just (floor vx, floor vy)
    verifyIntersect' [] Nothing = Nothing
    verifyIntersect' (x:xs) intersection =
      case (sameline, intersection) of
        (True, _) -> checkNext
        (False, Nothing) -> if isJust currIntersection then checkNext' else Nothing
        (False, Just _) -> if intersection == currIntersection then checkNext else Nothing
        where
          sameline = linesOverlap firstHail x
          checkNext = verifyIntersect' xs intersection
          currIntersection = solveEq firstHail x
          checkNext' = verifyIntersect' xs currIntersection



searchRockVelocity :: [Hailstone] -> Int -> Int -> Maybe ((Int, Int), (Int, Int))
searchRockVelocity hailstones from to = case uncons search of
  Nothing -> Nothing
  Just (x, _) -> Just x
  where
    searchRange = [(a * vx, b * vy) | vx <- lst, vy <- lst, a <- [1, -1], b <- [1, -1]]
    lst = [from..to]
    search = mapMaybe verifyRockVelocity searchRange

    -- search = catMaybes [verifyIntersect
    --        (map (shiftReferenceFrame2D vshift) hailstones) |
    --        vshift <- searchRange]
    -- search = filter (isJust . verifyRockVelocity ) map (shiftReferenceFrame2D` hailstones) 

    verifyRockVelocity :: (Int, Int) -> Maybe ((Int, Int), (Int, Int))
    verifyRockVelocity (vx, vy) = case result of
        Nothing -> Nothing
        Just (x, y) -> Just ((vx, vy), (x, y))
      where
        shiftedHails = map (shiftReferenceFrame2D (vx, vy)) hailstones
        result = verifyIntersect shiftedHails


trial input = searchRockVelocity hailstones 1 500
  where
    hailstones = parseHailstones input
    -- vshift = (-3, 1)
    -- shiftedLines = map vshift hailstones
    -- allPairs = makePairs $ map (shiftReferenceFrame2D vshift) hailstones



main = do
  testWithExample "24" trial
  -- runSolution 24 trial
  -- runSolution 24 day24part1