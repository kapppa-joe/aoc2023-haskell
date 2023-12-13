import Data.List (intercalate)
import Data.MemoTrie (memo2)
import Utils (runSolution)

type SpringIndex = Int

type NumGroupIndex = Int

parseLine :: String -> (String, [Int])
parseLine s = case words s of
  [springs, numbers] -> (springs, read $ "[" ++ numbers ++ "]")
  _ -> error "parse error"

solveLine :: String -> [Int] -> Int
solveLine springs numbers = solveLineMemo 0 0
  where
    solveLine' :: SpringIndex -> NumGroupIndex -> Int
    solveLine' a b =
      case (drop a springs, drop b numbers) of
        (xs, []) -> if '#' `elem` xs then 0 else 1
        ([], _) -> 0
        (x : xs, y : _) ->
          case x of
            '.' -> skip
            '#' -> take_
            '?' -> take_ + skip
            _ -> error "shouldn't have such char"
          where
            skip = solveLineMemo (a + 1) b
            take_ =
              let (springConsumed, remaining) = splitAt y (x : xs)
                  springsMatchNumber
                    | '.' `elem` springConsumed = False
                    | length springConsumed < y = False
                    | remaining /= [] && head remaining == '#' = False
                    | otherwise = True
                  finishThisGroupAndSolveNext = solveLineMemo (a + y + 1) (b + 1)
               in if springsMatchNumber then finishThisGroupAndSolveNext else 0

    solveLineMemo :: Int -> Int -> Int
    solveLineMemo = memo2 solveLine'

day12part1 :: [String] -> Int
day12part1 inputLines =
  let input = map parseLine inputLines
   in sum [solveLine springs numbers | (springs, numbers) <- input]

unfold :: (String, [Int]) -> (String, [Int])
unfold (springs, numbers) = (intercalate "?" $ replicate 5 springs, concat $ replicate 5 numbers)

day12part2 :: [String] -> Int
day12part2 inputLines =
  let input = map (unfold . parseLine) inputLines
   in sum [solveLine springs numbers | (springs, numbers) <- input]

main :: IO ()
main = do
  runSolution 12 day12part2
