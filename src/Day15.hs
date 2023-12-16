import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.Char (ord)
import Utils (runSolution)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

parseInput :: [String] -> [String]
parseInput s = splitOn ',' $ concat s

hash :: String -> Int
hash s = hash' s 0
  where
    hash' [] n = n
    hash' (x : xs) n =
      let n' = (n + ord x) * 17 `mod` 256
       in hash' xs n'

day15part1 :: [String] -> Int
day15part1 input =
  sum $ map hash $ parseInput input

type Label = String

type FocalLength = Int

type Lens = (Label, FocalLength)

type HASHMAP = Array Int [Lens]

data Operation = Assign Lens | Remove Label

parseOperation :: String -> Operation
parseOperation s =
  case splitOn '=' s of
    [label, num] -> Assign (label, read num)
    [xs] -> Remove $ takeWhile (/= '-') xs
    _ -> error "fail to parse operation"

parseOperations :: [String] -> [Operation]
parseOperations = map parseOperation . parseInput

removeLens :: Label -> [Lens] -> [Lens]
removeLens label xs =
  let (a, b) = break ((== label) . fst) xs
   in (a ++ drop 1 b)

assignLens :: Lens -> [Lens] -> [Lens]
assignLens lens@(label, _) xs =
  let (a, b) = break ((== label) . fst) xs
   in a ++ (lens : drop 1 b)

operate :: HASHMAP -> Operation -> HASHMAP
operate m op = case op of
  Assign lens@(label, _) -> updateBox label $ assignLens lens (getbox label)
  Remove label -> updateBox label $ removeLens label (getbox label)
  where
    getbox label = m ! hash label
    updateBox label lenses = m // [(hash label, lenses)]

focusingPower :: HASHMAP -> Int
focusingPower m = sum [getPower boxNum lenses | (boxNum, lenses) <- Array.assocs m]
  where
    getPower boxNum lenses =
      (boxNum + 1) * sum [idx * focalLength | (idx, (_, focalLength)) <- zip [1 ..] lenses]

day15part2 :: [String] -> Int
day15part2 input = focusingPower $ foldl operate emptyBoxes operations
  where
    emptyBoxes = Array.listArray (0, 255) $ repeat [] :: HASHMAP
    operations = parseOperations input

main :: IO ()
main = do
  runSolution 15 day15part2