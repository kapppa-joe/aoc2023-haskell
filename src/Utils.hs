module Utils
  ( runSolutionWithFile,
    runSolution,
    testWithExample,
    runWithParser,
    Solution
  )
where

import Text.Printf (printf)
import Text.ParserCombinators.Parsec (Parser, parseFromFile)

type Solution = [String] -> Integer
type Day = Int

runSolutionWithFile :: Solution -> String -> IO ()
runSolutionWithFile solution filename = do
  contents <- readFile filename
  let fileLines = lines contents
  let answer = solution fileLines
  print answer

runSolution :: Day -> Solution -> IO ()
runSolution day solution = runSolutionWithFile solution filename
  where
    filename = printf "puzzle/%02d.txt" day

testWithExample :: String -> Solution -> IO ()
testWithExample filename solution = runSolutionWithFile solution filename'
  where
    filename' = printf "example/" ++ filename ++ ".txt"

runWithParser :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithParser p solver fileName = do
  result <- parseFromFile p fileName
  case result of
    Left err -> print err
    Right games -> print $ solver games
