import Data.List (elemIndex, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

type Card = Char

type Hand = [Card]

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

allCards :: [Card]
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

type CardCounter = Map.Map Card Int

judgeHandType :: CardCounter -> TypeOfHand
judgeHandType counter
  | allSameCard = FiveOfAKind
  | hasXOfAKind 4 = FourOfAKind
  | hasXOfAKind 3 && hasXOfAKind 2 = FullHouse
  | hasXOfAKind 3 = ThreeOfAKind
  | countXOfAKind 2 == 2 = TwoPair
  | hasXOfAKind 2 = OnePair
  | otherwise = HighCard
  where
    hasXOfAKind x = not . Map.null $ Map.filter (== x) counter
    countXOfAKind x = Map.size $ Map.filter (== x) counter
    allSameCard = Map.size counter == 1

typeOfHand :: Hand -> TypeOfHand
typeOfHand = judgeHandType . countOccurence

cardStrength :: Card -> Int
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

day07part1 :: Input -> Int
day07part1 = solver handStrength

---------------
-- Part 2
---------------

cardStrengthJ :: Card -> Int
cardStrengthJ card = fromJust $ elemIndex card "J23456789TQKA"

judgeHandTypeJ :: CardCounter -> TypeOfHand
judgeHandTypeJ counter =
  case Map.lookup 'J' counter of
    Nothing -> judgeHandType counter -- reuse prev ver if no joker
    Just n -> handleJoker n
  where
    uniqueCards = Map.size counter
    hasXOfAKind x = not . Map.null $ Map.filter (== x) counter
    countXOfAKind x = Map.size $ Map.filter (== x) counter
    handleJoker jokerCount
      | uniqueCards <= 2 = FiveOfAKind
      | hasXOfAKind 3 = FourOfAKind -- AAABJ or ABJJJ
      | countXOfAKind 2 == 2 && jokerCount == 2 = FourOfAKind -- AABJJ
      | uniqueCards == 3 = FullHouse -- AABBJ
      | uniqueCards == 4 = ThreeOfAKind -- AABCJ
      | otherwise = OnePair

typeOfHandJ :: Hand -> TypeOfHand
typeOfHandJ = judgeHandTypeJ . countOccurence

handStrengthJ :: Hand -> HandStrength
handStrengthJ hand = (typeOfHandJ hand, [cardStrengthJ card | card <- hand])

day07part2 :: Input -> Int
day07part2 = solver handStrengthJ

bothParts :: Input -> (Int, Int)
bothParts x = (day07part1 x, day07part2 x)

main :: IO ()
main = runWithParser parseInput bothParts "puzzle/07.txt"
