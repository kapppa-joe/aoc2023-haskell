import Data.List (transpose, uncons)
import Data.Maybe (catMaybes)
import Utils (runSolution)

type Pattern = [String]

type SmudgeCount = Int

type LineNumber = Int

parsePatterns :: [String] -> [Pattern]
parsePatterns [] = []
parsePatterns xs =
  let (taken, rest) = span (/= "") xs
   in taken : parsePatterns (drop 1 rest)

extractPatternAsPairs :: Pattern -> LineNumber -> [(Char, Char)]
extractPatternAsPairs pattern x =
  let (top, bottom) = splitAt x pattern
   in zip (concat $ reverse top) (concat bottom)

countSmudge :: Pattern -> LineNumber -> Int
countSmudge pattern x = length [(a, b) | (a, b) <- extractPatternAsPairs pattern x, a /= b]

findReflectionLine :: SmudgeCount -> Pattern -> (Maybe LineNumber, Maybe LineNumber)
findReflectionLine n p = (findReflectionV p, findReflectionH p)
  where
    findReflectionH p' = fst <$> uncons [x | x <- [1 .. length p' - 1], countSmudge p' x == n]
    findReflectionV = findReflectionH . transpose

day13WithSmudgeCount :: SmudgeCount -> [String] -> Int
day13WithSmudgeCount n input =
  let (vLines, hLines) = unzip $ [findReflectionLine n p | p <- parsePatterns input]
   in (sum (catMaybes hLines) * 100) + sum (catMaybes vLines)

day13part1 :: [String] -> Int
day13part1 = day13WithSmudgeCount 0

day13part2 :: [String] -> Int
day13part2 = day13WithSmudgeCount 1

main :: IO ()
main = do
  runSolution 13 day13part2