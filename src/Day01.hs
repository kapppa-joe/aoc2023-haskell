import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Utils (runSolution)

numberWordList :: [String]
numberWordList = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

parseInput :: Bool -> [String] -> [[Int]]
parseInput considerWord = map parseLine
  where
    parseLine :: String -> [Int]
    parseLine [] = []
    parseLine str@(x : xs)
      | isDigit x = read [x] : parseRest
      | not considerWord = parseRest
      | otherwise = case numberWordFound of
          (n : _) -> n : parseRest
          _ -> parseRest
      where
        parseRest = parseLine xs
        numberWordFound = catMaybes [detectNumberWord str ws | ws <- zip numberWordList [1 .. 9]]

    detectNumberWord :: String -> (String, Int) -> Maybe Int
    detectNumberWord s (word, n)
      | take (length word) s == word = Just n
      | otherwise = Nothing

calibrationValue :: [Int] -> Int
calibrationValue [] = error "no digit found"
calibrationValue v@(x : _) = x * 10 + getLastDigit v
  where
    getLastDigit [y] = y
    getLastDigit [] = error "second digit not found"
    getLastDigit (_ : ys) = getLastDigit ys

day01part1 :: [String] -> Int
day01part1 input = sum $ map calibrationValue numbers
  where
    parseDigitsOnly = parseInput False
    numbers = parseDigitsOnly input

day01part2 :: [String] -> Int
day01part2 input = sum $ map calibrationValue numbers
  where
    parseWordsAsWell = parseInput True
    numbers = parseWordsAsWell input

main :: IO ()
main = do
  runSolution 1 day01part1
  runSolution 1 day01part2