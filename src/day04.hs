import Text.ParserCombinators.Parsec
import Data.List (intersect)


data Card = Card {
    id :: Int,
    winningNumbers :: [Int],
    numbersGot :: [Int]
} deriving (Show)


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
    where spaces = many1 $ char ' '

parseCards :: Parser [Card]
parseCards = sepBy1 parseCard $ char '\n'

s = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
parsed = parse parseCard "" s

-- main = do
--     case parsed of
--         Right card -> print $ countPoints card
--         Left _ -> print "err"


findWins :: Card -> [Int]
findWins (Card _ ws ns)  = intersect ws ns

countPoints :: Card -> Int
countPoints card = 
    let wins = length $ findWins card
        hasWon = wins > 0
    in
        if hasWon then 2 ^ (wins - 1) else 0
        

day04part01 :: [Card] -> Int
day04part01 cards = sum [countPoints card | card <- cards]

countWins :: [Card] -> [Int]
countWins = map $ length . findWins


day04part02 = countWins

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

winCard :: [Int] -> (Int -> Integer) -> Int -> Integer
winCard winsList f n
  | n >= length winsList = 0
  | winList !! n == 0 = 1
  | otherwise = 


        

runWithFile :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithFile p solver fileName = do
  result <- parseFromFile p fileName
  case result of
    Left err -> print err
    Right games -> print $ solver games

main = runWithFile parseCards day04part02 "example/04.txt"


