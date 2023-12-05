{-# LANGUAGE DuplicateRecordFields #-}

import Data.Char (isDigit)
import Utils

---------------
-- Parsing
---------------

data PartNumber = PartNumber
  { posX :: Int,
    posY :: Int,
    number :: Int
  }
  deriving (Show)

data Symbol = Symbol
  { posX :: Int,
    posY :: Int,
    symbol :: Char
  }
  deriving (Show)

data Schema = Schema [PartNumber] [Symbol] deriving (Show)

type Row = String

type PartialRow = String

emptySchema :: Schema
emptySchema = Schema [] []

joinSchema :: Schema -> Schema -> Schema
joinSchema (Schema a1 b1) (Schema a2 b2) = Schema (a1 ++ a2) (b1 ++ b2)

isEmpty :: Char -> Bool
isEmpty = (==) '.'

parseCell :: Int -> Int -> PartialRow -> Schema
parseCell _ _ [] = emptySchema
parseCell x y row@(cell : cells)
  | isEmpty cell = parseCell (x + 1) y cells
  | isDigit cell = parseForPart x y row
  | otherwise = parseForSymbol x y row

parseForPart :: Int -> Int -> PartialRow -> Schema
parseForPart x y row =
  let (numCells, remainingCells) = span isDigit row
      newNumber = read numCells :: Int
      newPart = PartNumber x y newNumber
      newX = x + length numCells
      Schema parts syms = parseCell newX y remainingCells
   in Schema (newPart : parts) syms

parseForSymbol :: Int -> Int -> PartialRow -> Schema
parseForSymbol _ _ [] = emptySchema
parseForSymbol x y (cell : cells) =
  let Schema parts syms = parseCell (x + 1) y cells
      newSymbol = Symbol x y cell
   in Schema parts (newSymbol : syms)

parseRow :: Int -> Row -> Schema
parseRow = parseCell 0

parseSchema :: [String] -> Schema
parseSchema = recur 0
  where
    recur :: Int -> [String] -> Schema
    recur _ [] = emptySchema
    recur y (currRow : rest) = joinSchema (parseRow y currRow) (recur (y + 1) rest)

-------------
-- PART 01
-------------

isAdjacent :: PartNumber -> Symbol -> Bool
isAdjacent (PartNumber x0 y0 num) (Symbol x1 y1 _)
  | abs (y1 - y0) > 1 = False
  | x0 - x1 > 1 = False
  | x1 - (x0 + countDigits num) > 0 = False
  | otherwise = True
  where
    countDigits = length . show

day03part1 :: Solution
day03part1 input =
  let (Schema partNums syms) = parseSchema input
      wantedParts = filter hasAdjacentSymbol partNums
      hasAdjacentSymbol num = any (isAdjacent num) syms
   in toInteger $ sum [number partNum | partNum <- wantedParts]

-------------
-- PART 02
-------------

getAdjacentParts :: [PartNumber] -> Symbol -> [PartNumber]
getAdjacentParts nums sym = [num | num <- nums, isAdjacent num sym]

day03part2 :: Solution
day03part2 input =
  let (Schema partNums symbols) = parseSchema input
      isGear = (== '*') . symbol
      gears = filter isGear symbols
      candidateParts = map (getAdjacentParts partNums) gears :: [[PartNumber]]
      wantedPairs = [pair | pair <- candidateParts, length pair == 2]
      getGearRatio pair = product [number part | part <- pair]
   in toInteger $ sum [getGearRatio pair | pair <- wantedPairs]

main :: IO ()
main = do
  runSolution 3 day03part2