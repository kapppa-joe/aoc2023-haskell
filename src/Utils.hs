module Utils
  ( runSolutionWithFile,
    runSolution,
    testWithExample,
    Solution
  )
where

import Text.Printf (printf)

type Solution = [String] -> Integer

runSolutionWithFile :: Solution -> String -> IO ()
runSolutionWithFile solution filename = do
  contents <- readFile filename
  let fileLines = lines contents
  let answer = solution fileLines
  print answer

runSolution :: Int -> Solution -> IO ()
runSolution day solution = runSolutionWithFile solution filename
  where
    filename = printf "puzzle/%02d.txt" day

testWithExample :: String -> Solution -> IO ()
testWithExample filename solution = runSolutionWithFile solution filename'
  where
    filename' = printf "example/" ++ filename ++ ".txt"
