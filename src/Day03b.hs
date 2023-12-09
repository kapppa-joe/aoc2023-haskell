{-# LANGUAGE DuplicateRecordFields #-}

import Data.Char (isDigit)
import Data.Either (lefts, rights)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

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

type NumberOrSymbol = Either PartNumber Symbol

data Schema = Schema [PartNumber] [Symbol] deriving (Show)

parsePartNumber :: Parser NumberOrSymbol
parsePartNumber = do
  num <- some digitChar
  SourcePos _ line col <- getSourcePos
  let x = unPos col - length num
      y = unPos line
      partNum = PartNumber x y (read num)
  return $ Left partNum

parseSymbol :: Parser NumberOrSymbol
parseSymbol = do
  n <- satisfy isSymbol
  SourcePos _ line col <- getSourcePos
  let x = unPos col - 1
      y = unPos line
  return $ Right $ Symbol x y n
  where
    isSymbol '.' = False
    isSymbol '\n' = False
    isSymbol x = not $ isDigit x

parseSchema :: Parser Schema
parseSchema = do
  _ <- many $ char '.'
  things <- sepEndBy (parsePartNumber <|> parseSymbol) (many $ oneOf ".\n")
  return $ Schema (lefts things) (rights things)

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

day03part1 :: Schema -> Int
day03part1 (Schema partNums syms) =
  let wantedParts = filter hasAdjacentSymbol partNums
      hasAdjacentSymbol num = any (isAdjacent num) syms
   in sum [number partNum | partNum <- wantedParts]

runWithParser :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithParser parser solver filename = do
  input <- TextIO.readFile filename
  let res = runParser parser "" input
  case res of
    Left err -> print err
    Right parsed -> print $ solver parsed

-------------
-- PART 02
-------------

getAdjacentParts :: [PartNumber] -> Symbol -> [PartNumber]
getAdjacentParts nums sym = [num | num <- nums, isAdjacent num sym]

day03part2 :: Schema -> Int
day03part2 (Schema partNums symbols) =
  let isGear = (== '*') . symbol
      gears = filter isGear symbols
      candidateParts = map (getAdjacentParts partNums) gears :: [[PartNumber]]
      wantedPairs = [pair | pair <- candidateParts, length pair == 2]
      getGearRatio pair = product [number part | part <- pair]
   in sum [getGearRatio pair | pair <- wantedPairs]

main :: IO ()
main = runWithParser parseSchema day03part2 "puzzle/03.txt"