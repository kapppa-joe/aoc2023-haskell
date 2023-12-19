{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Combinators (sepBy1, sepEndBy1)
import qualified Data.Map as Map
import Text.Megaparsec (choice, eof, some, try)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, lowerChar, string)
import UtilsM (Parser, runWithParser)
import Control.Monad.Combinators (sepEndBy)

data Parts = Parts {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Eq, Ord, Show)

data Criteria = X | M | A | S deriving (Eq, Ord, Enum, Show)

type Label = String

data Destination = Accept | Reject | Goto Label deriving (Eq, Ord, Show)

data Condition
  = GreaterThan Criteria Int
  | LesserThan Criteria Int
  | Else
  deriving (Eq, Ord, Show)

type Rule = (Condition, Destination)

type Workflow = [Rule]

type Workflows = Map.Map Label Workflow

parseWorkflow :: Parser (Label, Workflow)
parseWorkflow = do
  label <- some lowerChar
  _ <- char '{'
  rules <- sepEndBy (try parseRule) (char ',')
  endRule <- parseEndRule
  return (label, rules ++ [endRule])

parseBoth :: Parser (Workflows, [Parts])
parseBoth = do
  workflows <- parseWorkflows
  _ <- eol
  parts <- parseParts
  return (workflows, parts)

parseParts :: Parser [Parts]
parseParts = do
  parts <- sepBy1 parseSingleParts eol
  _ <- eof
  return parts
  where
    parseSingleParts :: Parser Parts
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
      return $ Parts x m a s

parseWorkflows :: Parser Workflows
parseWorkflows = do
  workflows <- sepEndBy1 parseWorkflow eol
  return (Map.fromList workflows)

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

main = do
  runWithParser parseBoth id "example/19.txt"