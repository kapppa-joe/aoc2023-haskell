import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

parseNums :: Parser [Int]
parseNums = do
  ns <- sepBy (many1 (digit <|> char '-')) (char ' ')
  return $ map read ns

parseNumLists :: Parser [[Int]]
parseNumLists = do
  sepBy parseNums endOfLine

extendList :: [Int] -> [Int]
extendList [] = []
extendList [x] = [x]
extendList numList@(x:_)
  | sameNumber numList = head numList : numList
  | otherwise = (head diffedList + x) : numList
  where
    diffedList = extendList $ zipWith (-) numList (tail numList)
    sameNumber [] = True
    sameNumber (y:ys) = all (== y) ys

day09part1 :: [[Int]] -> Int
day09part1 = day09part2 . map reverse

day09part2 :: [[Int]] -> Int
day09part2 numlists = sum [head $ extendList ns | ns <- numlists]


main :: IO ()
main = runWithParser parseNumLists day09part1 "puzzle/09.txt"