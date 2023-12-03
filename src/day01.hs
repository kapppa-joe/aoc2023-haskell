import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, isPrefixOf, tails)
import Data.Maybe (catMaybes)
import Utils (Solution, runSolution, testWithExample)

-- PART ONE

day01part1 :: Solution
day01part1 rows = toInteger $ sum calibrationValues
  where
    calibrationValues = map getCalibrationValue rows

getDigit :: String -> Maybe Int
getDigit [] = Nothing
getDigit (x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

getFirstDigit :: String -> Int
getFirstDigit xs = head $ catMaybes maybeDigits
  where
    maybeDigits = map getDigit $ tails xs

getCalibrationValue :: String -> Int
getCalibrationValue xs = headDigit * 10 + lastDigit
  where
    headDigit = getFirstDigit xs
    lastDigit = getFirstDigit $ reverse xs

-- PART TWO

digitsInWord = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

day01part2 :: Solution
day01part2 rows = toInteger $ sum calibrationValues
  where
    calibrationValues = map getCalibrationValue' rows

getDigit' :: String -> Maybe Int
getDigit' [] = Nothing
getDigit' v@(x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = tryGetDigitFromWord v

extractDigitWord :: String -> Maybe String
extractDigitWord xs = head' $ filter isPrefixOf' digitsInWord
  where
    isPrefixOf' word = word `isPrefixOf` xs
    head' [] = Nothing
    head' (x : _) = Just x

tryGetDigitFromWord :: String -> Maybe Int
tryGetDigitFromWord xs =
  case extractDigitWord xs of
    Just x -> (+ 1) <$> elemIndex x digitsInWord
    _ -> Nothing

getFirstDigit' :: String -> Int
getFirstDigit' s = head $ catMaybes maybeDigits
  where
    maybeDigits = map getDigit' $ tails s

getLastDigit' :: String -> Int
getLastDigit' s = head $ catMaybes maybeDigits
  where
    maybeDigits = reverse $ map getDigit' $ tails s

getCalibrationValue' :: String -> Int
getCalibrationValue' xs = firstDigit * 10 + lastDigit
  where
    firstDigit = getFirstDigit' xs
    lastDigit = getLastDigit' xs

main = do
  runSolution 1 day01part2

-- testWithExample "01" day01part2