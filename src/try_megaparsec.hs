{-# LANGUAGE DuplicateRecordFields #-}

-- import Control.Applicative
-- import Control.Monad

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

puzzle =
  concat
    [ "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]

data PartNumber = PartNumber
  { posX :: Int,
    posY :: Int,
    number :: Int
  }
  deriving (Show)

data Symbol = Symbol
  { posX :: Int,
    posY :: Int,
    symbol :: Char
  }
  deriving (Show)

data Thing = Either PartNumber Symbol

data Schema = Schema [PartNumber] [Symbol] deriving (Show)

parsePartNumber :: Parser (Either PartNumber Symbol)
parsePartNumber = do
  num <- some digitChar
  SourcePos _ line col <- getSourcePos
  let 
    partNum = PartNumber (unPos line) (unPos col - length num) (read num)
    a = Left partNum 
  return a

parseSymbol :: Parser Symbol
parseSymbol = do
  n <- satisfy (\x -> x /= '.' && (not . isDigit) x)
  return $ Symbol 1 1 n

parseSchema :: Parser [Thing]
parseSchema = do
  things <- sepBy (parsePartNumber <|> parseSymbol) (char '.')
  return things

main = parseTest parseSchema $ T.pack puzzle