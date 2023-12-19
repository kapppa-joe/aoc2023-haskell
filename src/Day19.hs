{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Combinators (sepBy1, sepEndBy, sepEndBy1)
import qualified Data.Array.IArray as IA
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Megaparsec (choice, eof, some, try)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, lowerChar, string)
import UtilsM (Parser, runWithParser)

type Parts = IA.Array (Int, Int) Int

data Criteria = X | M | A | S deriving (Eq, Ord, Enum, Show)

type Label = String

data Result = Accept | Reject | Goto Label deriving (Eq, Ord, Show)

data Condition
  = GreaterThan Criteria Int
  | LesserThan Criteria Int
  | Else
  deriving (Eq, Ord, Show)

type Rule = (Condition, Result)

type Workflow = [Rule]

type Workflows = Map.Map Label Workflow

parseBoth :: Parser (Workflows, Parts)
parseBoth = do
  workflows <- parseWorkflows
  _ <- eol
  parts <- parseParts
  return (workflows, parts)

parseRule :: Parser Rule
parseRule = do
  criteria <-
    choice
      [ X <$ char 'x',
        M <$ char 'm',
        A <$ char 'a',
        S <$ char 's'
      ]
  gtOrLt <- choice [GreaterThan <$ char '>', LesserThan <$ char '<']
  value <- some digitChar
  _ <- char ':'
  dest' <- some letterChar
  let condition = gtOrLt criteria (read value)
      dest = case dest' of
        "A" -> Accept
        "R" -> Reject
        label -> Goto label
  return (condition, dest)

parseEndRule :: Parser Rule
parseEndRule = do
  dest' <- some letterChar
  _ <- char '}'
  let dest = case dest' of
        "A" -> Accept
        "R" -> Reject
        label -> Goto label
  return (Else, dest)

parseWorkflow :: Parser (Label, Workflow)
parseWorkflow = do
  label <- some lowerChar
  _ <- char '{'
  rules <- sepEndBy (try parseRule) (char ',')
  endRule <- parseEndRule
  return (label, rules ++ [endRule])

parseWorkflows :: Parser Workflows
parseWorkflows = do
  workflows <- sepEndBy1 parseWorkflow eol
  return (Map.fromList workflows)

parseParts :: Parser Parts
parseParts = do
  partsList <- sepBy1 parseSingleParts eol
  _ <- eof
  let len = length partsList
      parts = IA.listArray ((1, 1), (len, 4)) $ concat partsList
  return parts
  where
    parseSingleParts :: Parser [Int]
    parseSingleParts = do
      _ <- string "{x="
      x <- read <$> some digitChar
      _ <- string ",m="
      m <- read <$> some digitChar
      _ <- string ",a="
      a <- read <$> some digitChar
      _ <- string ",s="
      s <- read <$> some digitChar
      _ <- char '}'
      return [x, m, a, s]

pickAccepted :: Workflows -> Parts -> [Int]
pickAccepted ws parts = filter (judgeByWorkflow "in") [1 .. totalPartsNum]
  where
    totalPartsNum = fst . snd $ IA.bounds parts

    judgeByWorkflow :: Label -> Int -> Bool
    judgeByWorkflow label partNum =
      let workflow = fromJust $ Map.lookup label ws
       in case performRule workflow partNum of
            Accept -> True
            Reject -> False
            Goto label' -> judgeByWorkflow label' partNum

    performRule :: [Rule] -> Int -> Result
    performRule [] _ = error "something went wrong"
    performRule ((cond, res) : xs) partsNum =
      case cond of
        Else -> res
        GreaterThan crit v -> if partsValue crit > v then res else next
        LesserThan crit v -> if partsValue crit < v then res else next
      where
        partsValue crit = parts IA.! (partsNum, fromEnum crit + 1)
        next = performRule xs partsNum

day19part1 :: (Workflows, Parts) -> Int
day19part1 (workflows, parts) = sumPartsRating $ pickAccepted workflows parts
  where
    sumPartsRating :: [Int] -> Int
    sumPartsRating accepted =
      sum $ [parts IA.! (partNum, cat) | partNum <- accepted, cat <- [1 .. 4]]


main :: IO ()
main = do
  runWithParser parseBoth day19part1 "puzzle/19.txt"