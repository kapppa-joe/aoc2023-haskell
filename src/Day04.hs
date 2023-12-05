module Day04
  ( day04part1,
    day04part2,
  )
where

import Data.List (intersect)
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

data Card = Card
  { id :: Int,
    winningNumbers :: [Int],
    numbersGot :: [Int]
  }
  deriving (Show)

---------------
-- Parsing
---------------

parseNumber :: Parser Int
parseNumber = do
  number <- many1 digit
  skipMany $ char ' '
  return $ read number

parseCard :: Parser Card
parseCard = do
  _ <- string "Card"
  spaces
  cardId <- many1 digit
  _ <- char ':'
  spaces
  winningNums <- many1 parseNumber
  _ <- string "|"
  spaces
  numsGot <- many1 parseNumber
  return $ Card (read cardId) winningNums numsGot

parseCards :: Parser [Card]
parseCards = sepBy1 parseCard endOfLine

---------------
-- Part 1
---------------

findWins :: Card -> [Int]
findWins (Card _ ws ns) = ws `intersect` ns

countPoints :: Card -> Int
countPoints card =
  let wins = length $ findWins card
      hasWon = wins > 0
   in if hasWon then 2 ^ (wins - 1) else 0

day04part1 :: [Card] -> Int
day04part1 cards = sum [countPoints card | card <- cards]

---------------
-- Part 2
---------------

type CardsOnHand = [Int]

countWins :: [Card] -> [Int]
countWins = map $ length . findWins

sumTwoLists :: [Int] -> [Int] -> [Int]
sumTwoLists a b = take maxLen $ zipWith (+) a' b'
  where
    maxLen = max (length a) (length b)
    a' = a ++ repeat 0
    b' = b ++ repeat 0

winCards :: [Card] -> CardsOnHand -> Int -> CardsOnHand
winCards cards cardsOnHand n
  | exceedBound = cardsOnHand
  | noCardsWon = cardsOnHand
  | otherwise = sumTwoLists cardsOnHand cardsWon
  where
    exceedBound = n >= length winCounts 
    winCounts = countWins cards
    noCardsWon = winCounts !! n == 0 
    wonHowMany = winCounts !! n :: Int
    howManyNthCardOnHand = cardsOnHand !! n :: Int
    nextNCardsWon = replicate wonHowMany howManyNthCardOnHand
    padZero = replicate (n + 1) 0
    cardsWon = padZero ++ nextNCardsWon

day04part2 :: [Card] -> Int
day04part2 cards = sum $ foldl winCards' startingCards [0 .. n]
  where
    n = length cards
    startingCards = replicate n 1
    winCards' = winCards cards

main :: IO ()
main = runWithParser parseCards day04part2 "puzzle/04.txt"
