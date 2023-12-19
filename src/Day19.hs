{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Combinators (sepBy1, sepEndBy, sepEndBy1)
import qualified Data.Array.IArray as IA
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.Megaparsec (choice, eof, some, try)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, lowerChar, string)
import UtilsM (Parser, runWithParser)

-------------------
-- Defs and parsers
-------------------

type PartNum = Int

type Parts = IA.Array (PartNum, Int) Int

data Category = X | M | A | S deriving (Eq, Ord, Enum, Show)

type Label = String

data Result = Accept | Reject | Goto Label deriving (Eq, Ord, Show)

data Condition
  = GreaterThan Category Int
  | LesserThan Category Int
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
  category <-
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
  let condition = gtOrLt category (read value)
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

-------------------
-- Part 1
-------------------

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
        GreaterThan cat v -> if partsValue cat > v then res else next
        LesserThan cat v -> if partsValue cat < v then res else next
      where
        partsValue cat = parts IA.! (partsNum, fromEnum cat + 1)
        next = performRule xs partsNum

day19part1 :: (Workflows, Parts) -> Int
day19part1 (workflows, parts) = sumPartsRating $ pickAccepted workflows parts
  where
    sumPartsRating :: [Int] -> Int
    sumPartsRating accepted =
      sum $ [parts IA.! (partNum, cat) | partNum <- accepted, cat <- [1 .. 4]]

-------------------
-- Part 2
-------------------

type Range = (Int, Int) -- Right exclusive

type PartRange = IA.Array Int Range -- length 4 arrays, as 4-tuples are not handy

discardNull :: Range -> Maybe Range
discardNull (a, b) = if a >= b then Nothing else Just (a, b)

-- spliting the range with condition n < x
-- divide into (truthyRange, falseyRange)
splitRangeLT :: Range -> Int -> (Maybe Range, Maybe Range)
splitRangeLT (a, b) x = (discardNull (a, min x b), discardNull (max a x, b))

combinePartRange :: PartRange -> Int -> Maybe Range -> Maybe PartRange
combinePartRange partRange categoryNumber maybeRange = case maybeRange of
  Nothing -> Nothing
  Just rng -> Just $ partRange IA.// [(categoryNumber, rng)]

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

splitByCondition :: PartRange -> Condition -> (Maybe PartRange, Maybe PartRange)
splitByCondition partRange Else = (Just partRange, Nothing)
splitByCondition partRange cond =
  case cond of
    LesserThan category value -> f category value
    --  for int a, a > x === not (a < x + 1)
    GreaterThan category value -> swap $ f category (value + 1)
  where
    f category value =
      let categoryNumber = fromEnum category + 1
          rangePicked = partRange IA.! categoryNumber
          rngs@(trueRange, falseRange) = splitRangeLT rangePicked value
          combinePartRange' = combinePartRange partRange categoryNumber
          combinedRanges = mapTuple combinePartRange' rngs
       in combinedRanges

runRuleWithRange :: Workflow -> PartRange -> [(Result, PartRange)]
runRuleWithRange [] _ = error "something went wrong"
runRuleWithRange ((cond, res) : xs) partRange = selected ++ recurResults
  where
    (passed, rejected) = splitByCondition partRange cond
    selected = case passed of
      Just rng -> [(res, rng)]
      Nothing -> []
    recurResults = case rejected of
      Just rng -> runRuleWithRange xs rng
      Nothing -> []

findAllAcceptedRanges :: Workflows -> [PartRange]
findAllAcceptedRanges ws = pickAcceptedRanges initial []
  where
    allRanges = IA.listArray (1, 4) $ repeat (1, 4001)
    initial = [(Goto "in", allRanges)]

    pickAcceptedRanges :: [(Result, PartRange)] -> [PartRange] -> [PartRange]
    pickAcceptedRanges [] accepted = accepted
    pickAcceptedRanges (x : xs) accepted =
      case x of
        (Accept, rng) -> pickAcceptedRanges xs (rng : accepted)
        (Reject, _) -> pickAcceptedRanges xs accepted
        (Goto label, rng) ->
          let nextWorkflow = fromJust $ Map.lookup label ws
              newResults = runRuleWithRange nextWorkflow rng
           in pickAcceptedRanges (xs ++ newResults) accepted

countDistinctParts :: PartRange -> Int
countDistinctParts rng = product [b - a | (a, b) <- IA.elems rng]

day19part2 :: (Workflows, b) -> Int
day19part2 inputs = sum $ map countDistinctParts $ findAllAcceptedRanges workflows
  where
    workflows = fst inputs

main :: IO ()
main = do
  runWithParser parseBoth day19part2 "puzzle/19.txt"