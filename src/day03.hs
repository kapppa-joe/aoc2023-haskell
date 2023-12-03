{-# LANGUAGE DuplicateRecordFields #-}

import Data.Char (isDigit)
import Utils (Solution, runSolution, testWithExample)


example :: [String]
example =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

data PartNumber = PartNumber
  { x :: Int,
    y :: Int,
    number :: Int
  }
  deriving (Show)

data Symbol = Symbol
  { x :: Int,
    y :: Int,
    symbol :: Char
  }
  deriving (Show)

data Schema = Schema {partNumbers :: [PartNumber], symbols :: [Symbol]} deriving (Show)

emptySchema = Schema [] []

join :: Schema -> Schema -> Schema
join (Schema a1 b1) (Schema a2 b2) = Schema (a1 ++ a2) (b1 ++ b2)

isEmpty :: Char -> Bool
isEmpty = (==) '.'

parseLineXY :: Int -> Int -> String -> Schema
parseLineXY x y [] = emptySchema
parseLineXY x y s@(cell : cells)
  | isEmpty cell = parseLineXY (x + 1) y cells
  | isDigit cell = parseLineP x y s
  | otherwise = parseLineS x y s

parseLineP x y s@(cell : cells) =
  let (numCells, remainingCells) = span isDigit s
      newNumber = read numCells :: Int
      newPart = PartNumber x y newNumber
      newX = x + length numCells
      Schema parts syms = parseLineXY newX y remainingCells
   in Schema (newPart : parts) syms

parseLineS x y (cell : cells) =
  let Schema parts syms = parseLineXY (x + 1) y cells
      newSymbol = Symbol x y cell
   in Schema parts (newSymbol : syms)

parseLineY :: Int -> String -> Schema
parseLineY y s = parseLineXY 0 y s

parseMap :: [String] -> Schema
parseMap strs = parseMap' 0 strs
  where
    parseMap' _ [] = emptySchema
    parseMap' y (curr : remaining) = join (parseLineY y curr) (parseMap' (y + 1) remaining)

-------------
-- PART 01
-------------

isAdjacent :: PartNumber -> Symbol -> Bool
isAdjacent (PartNumber x0 y0 number) (Symbol x1 y1 _)
  | abs (y1 - y0) > 1 = False
  | x0 - x1 > 1 = False
  | x1 - (x0 + countDigits number) > 0 = False
  | otherwise = True
  where
    countDigits = length . show

hasAdjacentSymbol :: [Symbol] -> PartNumber -> Bool
hasAdjacentSymbol syms num = any (isAdjacent num) syms

day03part1 :: Solution
day03part1 input =
  let (Schema partNums syms) = parseMap input
      wantedParts = filter (hasAdjacentSymbol syms) partNums
   in toInteger $ sum [number partNum | partNum <- wantedParts]


-------------
-- PART 02
-------------

getAdjacentParts :: [PartNumber] -> Symbol -> [PartNumber]
getAdjacentParts nums sym = [num | num <- nums, isAdjacent num sym]

day03part2 :: Solution
day03part2 input = 
  let 
    (Schema partNums syms) = parseMap input
    isGear sym = symbol sym == '*' 
    gears = filter isGear syms
    candidateParts = map (getAdjacentParts partNums) gears :: [[PartNumber]]
    wantedPairs = [pair | pair <- candidateParts, (length pair) == 2]
    getGearRatio pair = product [number part | part <- pair]
  in
    toInteger $ sum [getGearRatio pair | pair <- wantedPairs]

-- main = do
--   print $ day03part2 example

main = do
    runSolution 3 day03part2