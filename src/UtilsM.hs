
module UtilsM
  (
    runWithParser,
    debug,
    Parser
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO (readFile)
import Data.Void ( Void )
import Text.Megaparsec ( runParser, Parsec )
import Debug.Trace (trace)

type Parser = Parsec Void Text

runWithParser :: (Show a) => Parser t -> (t -> a) -> FilePath -> IO ()
runWithParser parser solver filename = do
  input <- TextIO.readFile filename
  let res = runParser parser "" input
  case res of 
    Left err -> print err
    Right parsed -> print $ solver parsed

debug :: c -> String -> c
debug = flip trace