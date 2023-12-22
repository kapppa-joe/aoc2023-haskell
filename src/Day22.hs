{-# LANGUAGE OverloadedRecordDot #-}

import Data.Char (isDigit)
import qualified Data.Ix as Ix
import Data.List (sortOn)
import Data.List.Split (splitWhen)
import Utils (testWithExample)

type Coord = (Int, Int, Int)

data Brick = Brick {from :: Coord, to :: Coord} deriving (Eq, Ord, Show, Read)

trd :: (a, b, c) -> c
trd (_, _, z) = z

highestZ :: Brick -> Int
highestZ b = max (trd b.from) (trd b.to)

lowestZ :: Brick -> Int
lowestZ b = min (trd b.from) (trd b.to)

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


handleFreeFall :: [Brick] -> [Brick]
handleFreeFall bricks = handleFreeFall' bricks []
  where
    handleFreeFall' :: [Brick] -> [Brick] -> [Brick]
    handleFreeFall' [] settled = settled
    handleFreeFall' (x:xs) settled = handleFreeFall' xs settled'
      where
        (nonTouching, supporters) = break (canSupport x) settled
        droppedX = case supporters of 
          [] -> dropToZLevel x 1
          (sp:_) -> dropToZLevel x $ highestZ sp + 1
        settled' = nonTouching ++ [droppedX] ++ supporters

    canSupport :: Brick -> Brick -> Bool
    canSupport brick supporter = overlap brick' supporter
      where
        brick' = dropToZLevel brick $ highestZ supporter

    dropToZLevel :: Brick -> Int -> Brick
    dropToZLevel brick z = Brick (x0, y0, z0 - dz) (x1, y1, z1 - dz)
      where
        dz = lowestZ brick - z
        ((x0, y0, z0), (x1, y1, z1)) = (brick.from, brick.to)




trial input = handleFreeFall bricks
  where
    bricks = parseBricks input
    


main = do
  testWithExample "22" trial
