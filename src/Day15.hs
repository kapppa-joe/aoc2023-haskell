import Data.Char (ord)
import qualified Data.Map as Map
import Utils (runSolution, testWithExample)

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

type LensBoxes = Map.Map Int [Lens]

data Operation = Assign Lens | Remove Label

parseOperation :: String -> Operation
parseOperation s =
  case splitOn '=' s of
    [label, num] -> Assign (label, read num)
    [xs] -> Remove $ takeWhile (/= '-') xs
    _ -> error "fail to parse operation"

parseInput' :: [String] -> [Operation]
parseInput' = map parseOperation . parseInput

removeLens :: Label -> Maybe [Lens] -> Maybe [Lens]
removeLens label xs = case xs of
  Nothing -> Nothing
  Just x@[(label', _)] -> if label == label' then Nothing else Just x
  Just x ->
    let (a, b) = break ((== label) . fst) x
     in Just (a ++ drop 1 b)

assignLens :: Lens -> Maybe [Lens] -> Maybe [Lens]
assignLens lens@(label, _) xs = case xs of
  Nothing -> Just [lens]
  Just x ->
    let (a, b) = break ((== label) . fst) x
     in Just (a ++ (lens : drop 1 b))

runHash :: LensBoxes -> Operation -> LensBoxes
runHash m (Remove label) = Map.alter (removeLens label) (hash label) m
runHash m (Assign lens) = Map.alter (assignLens lens) (hash $ fst lens) m

focusingPower :: LensBoxes -> Int
focusingPower m = sum [getPower boxNum lenses | (boxNum, lenses) <- Map.toList m]
  where
    getPower boxNum xs =
      (boxNum + 1) * sum [index * focalLength | (index, (_, focalLength)) <- zip [1 ..] xs]

day15part2 :: [String] -> Int
day15part2 input = focusingPower $ foldl runHash Map.empty operations
  where
    operations = parseInput' input

main :: IO ()
main = do
  runSolution 15 day15part2

-- testWithExample "15" day15part2