import Data.MemoTrie (memo2)
import Utils (debug, runSolution, runSolutionWithFile, testWithExample)
import Data.List (intercalate)

parseLine :: String -> (String, [Int])
parseLine s = case words s of
  [records, numbers] -> (records, read $ "[" ++ numbers ++ "]")
  _ -> error "parse error"

solveLine' :: String -> [Int] -> Int
solveLine' = memo2 solveLine
  where
    solveLine :: String -> [Int] -> Int
    solveLine springs numbers =
      case (springs, numbers) of
        (xs, []) -> if '#' `elem` xs then 0 else 1
        ([], _) -> 0
        (x : xs, y : ys) ->
          case x of
            '.' -> solveLine' xs numbers
            '#' ->
              let (consumed, remaining) = splitAt y springs
              in case () of
                    _ | any (`elem` ".") consumed -> 0
                    _ | length consumed < y -> 0
                    _ | remaining /= [] && head remaining == '#' -> 0
                    _ | otherwise -> solveLine' (drop 1 remaining) ys
            '?' ->
              solveLine' ('#' : xs) numbers + solveLine' ('.' : xs) numbers
            _ ->
              error "shouldn't have such char"

day12part1 inputLines =
  let input = map parseLine inputLines
   in sum $ [solveLine' springs numbers | (springs, numbers) <- input]

unfold :: (String, [Int]) -> (String, [Int])
unfold (springs, numbers) = (intercalate "?" $ replicate 5 springs, concat $ replicate 5 numbers) 

day12part2 inputLines = 
  let input = take 100 $ drop 1000 $ map (unfold . parseLine) inputLines
   in [solveLine' springs numbers | (springs, numbers) <- input]

main = do
  -- print $ solveLine "?.?" [2]
  -- print $ solveLine "###.??????#?.?" [3,1,2,2]
  -- print $ solveLine "#??#?.?" [2,2]
  runSolution 12 day12part2
  -- testWithExample "12" day12part2