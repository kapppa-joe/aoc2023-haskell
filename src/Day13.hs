import Data.List (transpose, uncons)
import Data.Maybe (catMaybes)
import Utils (runSolution, testWithExample)

type Pattern = [String]

parsePatterns :: [String] -> [Pattern]
parsePatterns [] = []
parsePatterns xs =
  let (taken, rest) = span (/= "") xs
   in taken : parsePatterns (drop 1 rest)

reflectionAt :: Pattern -> Int -> Bool
reflectionAt pattern x =
  let (top, bottom) = splitAt x pattern
      len = minimum $ map length [top, bottom]
      top' = take len $ reverse top
      bottom' = take len bottom
   in top' == bottom'

reflectionAt' :: Pattern -> Int -> Bool
reflectionAt' pattern x =
  let (top, bottom) = splitAt x pattern
      len = minimum $ map length [top, bottom]
      top' = concat $ take len $ reverse top
      bottom' = concat $ take len bottom
   in (== 1) $ length $ [(a, b) | (a, b) <- zip top' bottom', a /= b]

findReflectionH :: Pattern -> Maybe Int
findReflectionH p = fst <$> uncons [x | x <- [1 .. length p - 1], reflectionAt p x]

findReflectionV :: Pattern -> Maybe Int
findReflectionV = findReflectionH . transpose

findReflection :: Pattern -> (Maybe Int, Maybe Int)
findReflection p = (findReflectionV p, findReflectionH p)

day13part1 :: [String] -> Int
day13part1 input =
  let (vLines, hLines) = unzip $ [findReflection p | p <- parsePatterns input]
   in (sum (catMaybes hLines) * 100) + sum (catMaybes vLines)


findReflectionH' :: Pattern -> Maybe Int
findReflectionH' p = fst <$> uncons [x | x <- [1 .. length p - 1], reflectionAt' p x]

findReflectionV' :: Pattern -> Maybe Int
findReflectionV' = findReflectionH' . transpose

findReflection' :: Pattern -> (Maybe Int, Maybe Int)
findReflection' p = (findReflectionV' p, findReflectionH' p)


-- day13part2 :: [String] -> Int
-- day13part2 input = [findReflection' p | p <- parsePatterns input]
  

day13part2 :: [String] -> Int
day13part2 input =
  let (vLines, hLines) = unzip $ [findReflection' p | p <- parsePatterns input]
   in (sum (catMaybes hLines) * 100) + sum (catMaybes vLines)

  --  in (sum (catMaybes hLines) * 100) + sum (catMaybes vLines)


main = do
  -- testWithExample "13" day13part2
  runSolution 13 day13part2