import Data.List (elemIndex, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

type Label = Char

type Hand = [Label]

type Bid = Int

type Input = [(Hand, Bid)]

data TypeOfHand
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Enum, Show)

allCards :: [Label]
allCards = "23456789TJQKA"

parseInput :: Parser Input
parseInput = do
  sepBy1 parseInputLine endOfLine
  where
    parseInputLine :: Parser (Hand, Bid)
    parseInputLine = do
      hand <- many1 $ oneOf allCards
      _ <- space
      bid <- many1 digit
      return (hand, read bid)

---------------
-- Part 1
---------------

countOccurence :: (Ord a) => [a] -> Map.Map a Int
countOccurence [] = Map.empty
countOccurence (key : xs) =
  let m = countOccurence xs
   in case Map.lookup key m of
        Nothing -> Map.insert key 1 m
        Just _ -> Map.insertWith (+) key 1 m

type CardCounter = Map.Map Label Int

judgeHandType :: CardCounter -> TypeOfHand
judgeHandType counter
  | uniqueCards == 1 = FiveOfAKind
  | uniqueCards == 2 = if hasXOfAKind 4 then FourOfAKind else FullHouse
  | uniqueCards == 3 = if hasXOfAKind 3 then ThreeOfAKind else TwoPair
  | uniqueCards == 4 = OnePair
  | otherwise = HighCard
  where
    hasXOfAKind x = not . Map.null $ Map.filter (== x) counter
    uniqueCards = Map.size counter

typeOfHand :: Hand -> TypeOfHand
typeOfHand = judgeHandType . countOccurence

cardStrength :: Label -> Int
cardStrength card = fromJust $ elemIndex card allCards

type HandStrength = (TypeOfHand, [Int])

handStrength :: Hand -> HandStrength
handStrength hand = (typeOfHand hand, [cardStrength card | card <- hand])

solver :: (Hand -> HandStrength) -> [(Hand, Bid)] -> Int
solver sortKeyFunc handsAndBids =
  let sorted = sortOn (sortKeyFunc . fst) handsAndBids
      handsWithRank = zip sorted [1 ..] :: [((Hand, Bid), Int)]
      winnings = [bid * rank | ((_, bid), rank) <- handsWithRank]
   in sum winnings

day07part1 :: [(Hand, Bid)] -> Int
day07part1 = solver handStrength

---------------
-- Part 2
---------------

cardStrengthJ :: Label -> Int
cardStrengthJ card = fromJust $ elemIndex card "J23456789TQKA"

judgeHandTypeJ :: CardCounter -> TypeOfHand
judgeHandTypeJ counter =
  case Map.lookup 'J' counter of
    Nothing -> judgeHandType counter
    Just n -> handleJoker n
  where
    uniqueCards = Map.size counter
    hasXOfAKind x = not . Map.null $ Map.filter (== x) counter
    handleJoker jokerCount
      | uniqueCards <= 2 = FiveOfAKind
      | uniqueCards == 3 && jokerCount >= 2 = FourOfAKind
      | uniqueCards == 3 && hasXOfAKind 3 = FourOfAKind
      | uniqueCards == 3 = FullHouse
      | uniqueCards == 4 = ThreeOfAKind
      | otherwise = OnePair

typeOfHandJ :: Hand -> TypeOfHand
typeOfHandJ = judgeHandTypeJ . countOccurence

handStrengthJ :: Hand -> HandStrength
handStrengthJ hand = (typeOfHandJ hand, [cardStrengthJ card | card <- hand])

day07part2 :: [(Hand, Bid)] -> Int
day07part2 = solver handStrengthJ

main :: IO ()
main = runWithParser parseInput day07part2 "puzzle/07.txt"
