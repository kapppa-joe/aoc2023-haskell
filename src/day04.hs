import Data.List (intersect)
import Text.ParserCombinators.Parsec

data Card = Card
  { id :: Int,
    winningNumbers :: [Int],
    numbersGot :: [Int]
  }
  deriving (Show)

parseNumber :: Parser Int
parseNumber = do
  number <- many1 digit
  many $ char ' '
  return $ read number

parseCard :: Parser Card
parseCard = do
  string "Card"
  spaces
  cardId <- many1 digit
  char ':'
  spaces
  winningNumbers <- many1 parseNumber
  string "|"
  spaces
  numbersGot <- many1 parseNumber
  return $ Card (read cardId) winningNumbers numbersGot
  where
    spaces = many1 $ char ' '

parseCards :: Parser [Card]
parseCards = sepBy1 parseCard $ char '\n'

findWins :: Card -> [Int]
findWins (Card _ ws ns) = intersect ws ns

countPoints :: Card -> Int
countPoints card =
  let wins = length $ findWins card
      hasWon = wins > 0
   in if hasWon then 2 ^ (wins - 1) else 0

day04part01 :: [Card] -> Int
day04part01 cards = sum [countPoints card | card <- cards]

countWins :: [Card] -> [Int]
countWins = map $ length . findWins

sumTwoLists :: [Integer] -> [Integer] -> [Integer]
sumTwoLists a b = take maxLen $ zipWith (+) a' b'
  where
    maxLen = max (length a) (length b)
    a' = a ++ repeat 0
    b' = b ++ repeat 0

winCards :: [Card] -> [Integer] -> Int -> [Integer]
winCards cards cardCounter n
  | n >= length winCounts = cardCounter
  | winCounts !! n == 0 = cardCounter
  | otherwise = sumTwoLists cardCounter cardsWon
  where
    winCounts = countWins cards
    wonHowMany = winCounts !! n :: Int
    howManyNthCard = cardCounter !! n :: Integer
    cardsWon' = replicate wonHowMany howManyNthCard
    padZero = replicate (n + 1) 0
    cardsWon = padZero ++ cardsWon'

day04part02 :: [Card] -> Integer
day04part02 cards = sum $ foldl winCards' startingCards [0 .. n]
  where
    n = length cards
    startingCards = replicate n 1
    winCards' = winCards cards

runWithFile :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithFile p solver fileName = do
  result <- parseFromFile p fileName
  case result of
    Left err -> print err
    Right games -> print $ solver games

main = runWithFile parseCards day04part02 "puzzle/04.txt"
