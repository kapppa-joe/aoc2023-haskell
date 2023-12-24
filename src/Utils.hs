module Utils
  ( runSolutionWithFile,
    runSolution,
    testWithExample,
    testWithExample',
    runWithParser,
    debug,
    fst',
    snd',
    trd,
    Solution,
  )
where

import Debug.Trace (trace)
import Text.ParserCombinators.Parsec (Parser, parseFromFile)
import Text.Printf (printf)

type Solution = [String] -> Integer

type Day = Int

runSolutionWithFile :: (Show a) => ([String] -> a) -> FilePath -> IO ()
runSolutionWithFile solution filename = do
  contents <- readFile filename
  let fileLines = lines contents
  let answer = solution fileLines
  print answer

runSolutionWithFile' :: ([String] -> IO ()) -> FilePath -> IO ()
runSolutionWithFile' solveAndPrint filename = do
  contents <- readFile filename
  let fileLines = lines contents
  solveAndPrint fileLines

runSolution :: (Show a) => Day -> ([String] -> a) -> IO ()
runSolution day solution = runSolutionWithFile solution filename
  where
    filename = printf "puzzle/%02d.txt" day

testWithExample :: (Show a) => FilePath -> ([String] -> a) -> IO ()
testWithExample filename solution = runSolutionWithFile solution filename'
  where
    filename' = printf "example/" ++ filename ++ ".txt"

testWithExample' :: FilePath -> ([String] -> IO ()) -> IO ()
testWithExample' filename solution = runSolutionWithFile' solution filename'
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

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

trd :: (a, b, c) -> c
trd (_, _, z) = z