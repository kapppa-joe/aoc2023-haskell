{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Array.IArray as IA
import Data.Char (isDigit)
import qualified Data.Ix as Ix
import Data.List (sortOn)
import Data.List.Split (splitWhen)
import qualified Data.Map as Map
import qualified Data.Ord
import Utils (runSolution, testWithExample)

type Coord = (Int, Int, Int)

data Brick = Brick {from :: Coord, to :: Coord} deriving (Eq, Ord, Show)

type SettledBricks = IA.Array Int (Brick, Int, Int)

-- store array idx
data Dependency = D {topper :: Int, supporter :: Int} deriving (Eq, Ord, Show)

type DependenciesGraph = Map.Map Int [Int] -- (key: supporter, vals: bricks on top)

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

buildDependencies :: SettledBricks -> [Dependency]
buildDependencies arr = buildDependencies' [1 .. len] []
  where
    (_, len) = IA.bounds arr

    getInfo :: Int -> (Brick, Int, Int)
    getInfo x = arr IA.! x

    getBrick :: Int -> Brick
    getBrick = fst' . getInfo

    getHighZ :: Int -> Int
    getHighZ = snd' . getInfo

    buildDependencies' :: [Int] -> [Dependency] -> [Dependency]
    buildDependencies' [] done = done
    buildDependencies' (currIndex : rest) done = buildDependencies' rest (newlyAdded ++ done)
      where
        (curr, highZ, lowZ) = arr IA.! currIndex
        (_, lowerBricks) = span ((>= lowZ) . getHighZ) rest
        (candidates, _) = span ((== lowZ - 1) . getHighZ) lowerBricks
        supporters = [s | s <- candidates, isSupporting curr $ getBrick s]
        newlyAdded = [D {topper = currIndex, supporter = s} | s <- supporters]

    isSupporting :: Brick -> Brick -> Bool
    isSupporting curr = overlap ifFallBy1
      where
        ifFallBy1 = fallBy 1 curr

-- verify :: [Brick] -> Bool
-- verify xs = not $ any (uncurry overlap) [(a, b) | a <- xs, b <- xs, a /= b]

trial input = buildDependencies $ handleFreeFall bricks
  where
    bricks = parseBricks input

main = do
  testWithExample "22" trial

-- runSolution 22 trial