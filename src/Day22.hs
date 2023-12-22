{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Array.IArray as IA
import Data.Char (isDigit)
import qualified Data.Ix as Ix
import Data.List (sortOn)
import Data.List.Split (splitWhen)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Ord
import qualified Data.Set as Set
import Utils (runSolution, testWithExample)

type Coord = (Int, Int, Int)

data Brick = Brick {from :: Coord, to :: Coord} deriving (Eq, Ord, Show)

type BrickIndex = Int

type SettledBricks = IA.Array BrickIndex (Brick, Int, Int)

data Dependency = Depend {supporting :: [BrickIndex], supportedBy :: [BrickIndex]} deriving (Eq, Ord, Show)

type DependenciesGraph = Map.Map BrickIndex Dependency

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

trd :: (a, b, c) -> c
trd (_, _, z) = z

highestZ :: Brick -> Int
highestZ b = max (trd b.from) (trd b.to)

lowestZ :: Brick -> Int
lowestZ b = min (trd b.from) (trd b.to)

lowestLevel :: Int
lowestLevel = 1

parseBricks :: [String] -> [Brick]
parseBricks input = sortOn lowestZ $ map parseBrick input
  where
    parseBrick line = case coords of
      [a, b] -> if a <= b then Brick a b else Brick b a
      _ -> error "failed to parse input"
      where
        coords :: [Coord]
        coords = toTriples $ map read $ splitWhen (not . isDigit) line

        toTriples :: [Int] -> [Coord]
        toTriples xs = case take 3 xs of
          [] -> []
          [x, y, z] -> (x, y, z) : toTriples (drop 3 xs)
          _ -> error "failed to parse coord"

overlap :: Brick -> Brick -> Bool
overlap a b = any overlapWithA pointsInB
  where
    pointsInB = Ix.range (b.from, b.to)
    overlapWithA = Ix.inRange (a.from, a.to)

fallBy :: Int -> Brick -> Brick
fallBy dz brick = Brick (x0, y0, z0 - dz) (x1, y1, z1 - dz)
  where
    ((x0, y0, z0), (x1, y1, z1)) = (brick.from, brick.to)

handleFreeFall :: [Brick] -> SettledBricks
handleFreeFall bricks = toArray $ handleFreeFall' bricks []
  where
    handleFreeFall' :: [Brick] -> [Brick] -> [Brick]
    handleFreeFall' [] settled = settled
    handleFreeFall' (x : xs) settled = handleFreeFall' xs settled'
      where
        (nonTouching, supporters) = break (canSupport x) settled
        droppedX = case supporters of
          [] -> dropToZLevel x lowestLevel
          (sp : _) -> dropToZLevel x $ highestZ sp + 1
        settled' = sortOn (Data.Ord.Down . highestZ) (nonTouching ++ [droppedX] ++ supporters)

    canSupport :: Brick -> Brick -> Bool
    canSupport brick supporter = overlap brick' supporter
      where
        brick' = dropToZLevel brick $ highestZ supporter

    dropToZLevel :: Brick -> Int -> Brick
    dropToZLevel brick z = Brick (x0, y0, z0 - dz) (x1, y1, z1 - dz)
      where
        dz = lowestZ brick - z
        ((x0, y0, z0), (x1, y1, z1)) = (brick.from, brick.to)

    toArray :: [Brick] -> SettledBricks
    toArray settled = IA.listArray (1, length settled) [(brick, highestZ brick, lowestZ brick) | brick <- settled]

emptyDependency :: Dependency
emptyDependency = Depend [] []

buildDependencies :: SettledBricks -> DependenciesGraph
buildDependencies arr = foldl addDependency Map.empty $ buildDependencies' [1 .. len] []
  where
    (_, len) = IA.bounds arr

    getInfo :: BrickIndex -> (Brick, Int, Int)
    getInfo x = arr IA.! x

    getBrick :: BrickIndex -> Brick
    getBrick = fst' . getInfo

    getHighZ :: BrickIndex -> Int
    getHighZ = snd' . getInfo

    isSupporting :: Brick -> Brick -> Bool
    isSupporting curr = overlap ifFallBy1
      where
        ifFallBy1 = fallBy 1 curr

    buildDependencies' :: [BrickIndex] -> [(BrickIndex, BrickIndex)] -> [(BrickIndex, BrickIndex)] -- (on top, supporting)
    buildDependencies' [] done = done
    buildDependencies' (currIndex : rest) done = buildDependencies' rest (newlyAdded ++ done)
      where
        (curr, highZ, lowZ) = arr IA.! currIndex
        (_, lowerBricks) = span ((>= lowZ) . getHighZ) rest
        (candidates, _) = span ((== lowZ - 1) . getHighZ) lowerBricks
        supporters = [s | s <- candidates, isSupporting curr $ getBrick s]
        newlyAdded = [(currIndex, supporter) | supporter <- supporters]

    addDependency :: DependenciesGraph -> (BrickIndex, BrickIndex) -> DependenciesGraph
    addDependency m (onTop, supporter) = update m
      where
        left = Map.findWithDefault emptyDependency onTop m
        right = Map.findWithDefault emptyDependency supporter m
        left' = left {supportedBy = supporter : left.supportedBy}
        right' = right {supporting = onTop : right.supporting}
        update = Map.insert onTop left' . Map.insert supporter right'

isSoleSupporter :: DependenciesGraph -> BrickIndex -> Bool
isSoleSupporter dependencies x = case supportedBricks of
  [] -> False
  xs -> any ((== 1) . countSupporters) xs
  where
    dependency = Map.findWithDefault emptyDependency x dependencies
    supportedBricks = dependency.supporting

    countSupporters :: BrickIndex -> Int
    countSupporters n = length (fromJust $ Map.lookup n dependencies).supportedBy

day22part1 :: [String] -> Int
day22part1 input = length $ filter canDestroy $ IA.indices settledBricks
  where
    settledBricks = handleFreeFall $ parseBricks input
    dependencies = buildDependencies settledBricks

    canDestroy :: BrickIndex -> Bool
    canDestroy x = not $ isSoleSupporter dependencies x

day22part2 :: [String] -> Int
day22part2 input = sum $ map countChainReaction candidates
  where
    settledBricks = handleFreeFall $ parseBricks input
    dependencies = buildDependencies settledBricks
    isSoleSupporter' = isSoleSupporter dependencies
    candidates = filter isSoleSupporter' $ IA.indices settledBricks

    findSupporters :: BrickIndex -> [BrickIndex]
    findSupporters x = (Map.findWithDefault emptyDependency x dependencies).supportedBy

    findSupported :: BrickIndex -> [BrickIndex]
    findSupported x = (Map.findWithDefault emptyDependency x dependencies).supporting

    countChainReaction :: BrickIndex -> Int
    countChainReaction x = Set.size (chainReaction mayFall fallen) - 1
      where
        mayFall = findSupported x
        fallen = Set.singleton x

    chainReaction :: [BrickIndex] -> Set.Set BrickIndex -> Set.Set BrickIndex
    chainReaction [] fallen = fallen
    chainReaction mayFall fallen = chainReaction nextCandidates fallen'
      where
        lostAllSupporter = all (`Set.member` fallen) . findSupporters
        confirmedFall = filter lostAllSupporter mayFall
        nextCandidates = concatMap findSupported confirmedFall
        fallen' = Set.union fallen (Set.fromList confirmedFall)

main :: IO ()
main = do
  runSolution 22 day22part2
