import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, isPrefixOf, tails)
import Data.Maybe (catMaybes)
import Utils (Solution, runSolution)

--------------
-- PART ONE
--------------

type Row = String

day01part1 :: Solution
day01part1 rows = toInteger $ sum calibrationValues
  where
    calibrationValues = map getCalibrationValue rows

getDigit :: Row -> Maybe Int
getDigit [] = Nothing
getDigit (x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

getFirstDigit :: Row -> Int
getFirstDigit xs = head $ catMaybes maybeDigits
  where
    maybeDigits = map getDigit $ tails xs

getCalibrationValue :: Row -> Int
getCalibrationValue xs = headDigit * 10 + lastDigit
  where
    headDigit = getFirstDigit xs
    lastDigit = getFirstDigit $ reverse xs

--------------
-- PART TWO
--------------

digitsInWord :: [String]
digitsInWord = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

day01part2 :: Solution
day01part2 rows = toInteger $ sum calibrationValues
  where
    calibrationValues = map getCalibrationValue' rows

getDigit' :: Row -> Maybe Int
getDigit' [] = Nothing
getDigit' row@(x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = tryParseNumberWord row

findNumberWord :: Row -> Maybe String
findNumberWord row = safeHead $ filter isPrefixOf' digitsInWord
  where
    isPrefixOf' word = word `isPrefixOf` row
    safeHead [] = Nothing
    safeHead (x : _) = Just x

tryParseNumberWord :: Row -> Maybe Int
tryParseNumberWord row = findNumberWord row >>= wordToNumber
  where
    wordToNumber word = (+ 1) <$> elemIndex word digitsInWord

getFirstDigit' :: Row -> Int
getFirstDigit' row = head $ catMaybes maybeDigits
  where
    maybeDigits = map getDigit' $ tails row

getLastDigit' :: Row -> Int
getLastDigit' row = head $ catMaybes maybeDigits
  where
    maybeDigits = reverse $ map getDigit' $ tails row

getCalibrationValue' :: Row -> Int
getCalibrationValue' row = firstDigit * 10 + lastDigit
  where
    firstDigit = getFirstDigit' row
    lastDigit = getLastDigit' row

main :: IO ()
main = do
  runSolution 1 day01part2
