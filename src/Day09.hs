-- import Text.Parsec
-- import Text.ParserCombinators.Parsec (Parser)
-- import Utils (runWithParser)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO (readFile)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseNum :: Parser Int
parseNum = L.signed nospace L.decimal

nospace :: Parser ()
nospace = do return ()

parseNums :: Parser [Int]
parseNums = sepBy1 parseNum hspace

parseNumLists :: Parser [[Int]]
parseNumLists = sepBy1 parseNums eol

main = do
  text <- TextIO.readFile "example/09.txt"
  let res = runParser parseNumLists "" text
  case res of 
    Left err -> print err
    Right result -> print result


-- extendList :: [Int] -> [Int]
-- extendList [] = []
-- extendList [x] = [x]
-- extendList numList@(x : xs)
--   | allSameNumber = x : numList
--   | otherwise = (head diffedList + x) : numList
--   where
--     diffedList = extendList $ zipWith (-) numList (tail numList)
--     allSameNumber = all (== x) xs

-- day09part1 :: [[Int]] -> Int
-- day09part1 = day09part2 . map reverse

-- day09part2 :: [[Int]] -> Int
-- day09part2 numlists = sum [head $ extendList ns | ns <- numlists]

-- main :: IO ()
-- main = runWithParser parseNumLists day09part1 "puzzle/09.txt"