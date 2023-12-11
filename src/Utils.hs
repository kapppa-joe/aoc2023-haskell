module Utils
  ( runSolutionWithFile,
    runSolution,
    testWithExample,
    runWithParser,
    debug,
    Solution
  )
where

import Text.Printf (printf)
import Text.ParserCombinators.Parsec (Parser, parseFromFile)
import Debug.Trace (trace)

type Solution = [String] -> Integer
type Day = Int

runSolutionWithFile :: Show a => ([String] -> a) -> FilePath -> IO ()
runSolutionWithFile solution filename = do
  contents <- readFile filename
  let fileLines = lines contents
  let answer = solution fileLines
  print answer

runSolution :: Show a => Day -> ([String] -> a) -> IO ()
runSolution day solution = runSolutionWithFile solution filename
  where
    filename = printf "puzzle/%02d.txt" day

testWithExample :: Show a => FilePath -> ([String] -> a) -> IO ()
testWithExample filename solution = runSolutionWithFile solution filename'
  where
    filename' = printf "example/" ++ filename ++ ".txt"

runWithParser :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithParser p solver fileName = do
  result <- parseFromFile p fileName
  case result of
    Left err -> print err
    Right games -> print $ solver games

debug :: c -> String -> c
debug = flip trace