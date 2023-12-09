import Data.Text (Text)
-- import qualified Data.Text as T
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


runWithParser :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithParser parser solver filename = do
  input <- TextIO.readFile filename
  let res = runParser parser "" input
  case res of 
    Left err -> print err
    Right parsed -> print $ solver parsed


extendList :: [Int] -> [Int]
extendList [] = []
extendList [x] = [x]
extendList numList@(x : xs)
  | allSameNumber = x : numList
  | otherwise = (head diffedList + x) : numList
  where
    diffedList = extendList $ zipWith (-) numList (tail numList)
    allSameNumber = all (== x) xs

day09part1 :: [[Int]] -> Int
day09part1 = day09part2 . map reverse

day09part2 :: [[Int]] -> Int
day09part2 numlists = sum [head $ extendList ns | ns <- numlists]

main :: IO ()
main = runWithParser parseNumLists day09part1 "puzzle/09.txt"