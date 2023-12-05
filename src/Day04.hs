module Day04
  ( day04part1,
    day04part2,
  )
where

import Data.List (intersect)
import Text.Parsec (endOfLine)
import Text.ParserCombinators.Parsec
import Utils (runWithParser)

data Card = Card
  { id :: Int,
    winningNumbers :: [Int],
    numbersGot :: [Int]
  }
  deriving (Show)

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

findWins :: Card -> [Int]
findWins (Card _ ws ns) = ws `intersect` ns

countPoints :: Card -> Int
countPoints card =
  let wins = length $ findWins card
      hasWon = wins > 0
   in if hasWon then 2 ^ (wins - 1) else 0

day04part1 :: [Card] -> Int
day04part1 cards = sum [countPoints card | card <- cards]

countWins :: [Card] -> [Int]
countWins = map $ length . findWins

sumTwoLists :: [Integer] -> [Integer] -> [Integer]
sumTwoLists a b = take maxLen $ zipWith (+) a' b'
  where
    maxLen = max (length a) (length b)
    a' = a ++ repeat 0
    b' = b ++ repeat 0

winCards :: [Card] -> [Integer] -> Int -> [Integer]
winCards cards cardsCounter n
  | n >= length winCounts = cardsCounter
  | winCounts !! n == 0 = cardsCounter
  | otherwise = sumTwoLists cardsCounter cardsWon
  where
    winCounts = countWins cards
    wonHowMany = winCounts !! n :: Int
    nthCardOnHand = cardsCounter !! n :: Integer
    cardsWon' = replicate wonHowMany nthCardOnHand
    padZero = replicate (n + 1) 0
    cardsWon = padZero ++ cardsWon'

day04part2 :: [Card] -> Integer
day04part2 cards = sum $ foldl winCards' startingCards [0 .. n]
  where
    n = length cards
    startingCards = replicate n 1
    winCards' = winCards cards

main :: IO ()
main = runWithParser parseCards day04part2 "puzzle/04.txt"