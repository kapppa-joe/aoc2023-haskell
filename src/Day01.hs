import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Utils (runSolution)

data Number = Digit Int | Word Int deriving (Eq, Ord, Show)

numberWordList :: [String]
numberWordList = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

parseInput :: Bool -> [String] ->  [[Number]]
parseInput considerWord = map parseLine
  where
    parseLine :: String -> [Number]
    parseLine [] = []
    parseLine str@(x : xs)
      | isDigit x = Digit (read [x]) : parseRest
      | not considerWord = parseRest
      | otherwise = case numberWordFound of
          (n : _) -> Word n : parseRest
          _ -> parseRest
      where
        parseRest = parseLine xs
        numberWordFound = catMaybes [detectNumberWord str ws | ws <- zip numberWordList [1..9]]

    detectNumberWord :: String -> (String, Int) -> Maybe Int
    detectNumberWord s (word, n)
      | take (length word) s == word = Just n
      | otherwise = Nothing


value :: Number -> Int
value (Digit x) = x
value (Word x) = x

calibrationValue :: [Number] -> Int
calibrationValue [] = error "no digit found"
calibrationValue v@(x:_) = value x * 10 + getLastDigit v
  where
    getLastDigit [y] = value y
    getLastDigit [] = error "second digit not found"
    getLastDigit (_:ys) = getLastDigit ys


day01part1 :: [String] -> Int
day01part1 input = sum $ map calibrationValue numbers
  where
    parseDigitsOnly = parseInput False
    numbers =  parseDigitsOnly input

day01part2 :: [String] -> Int
day01part2 input = sum $ map calibrationValue numbers
  where
    parseWordsAsWell = parseInput True
    numbers = parseWordsAsWell input


main :: IO ()
main = do
  runSolution 1 day01part1
  runSolution 1 day01part2