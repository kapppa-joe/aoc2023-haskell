import Text.ParserCombinators.Parsec
import Utils (runWithParser)

data Colour = Red | Green | Blue deriving (Eq, Ord, Enum, Show)

data Draw = Draw
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

data Game = Game
  { gameId :: Int,
    draws :: [Draw]
  }
  deriving (Show)

emptyDraw = Draw {red = 0, green = 0, blue = 0}

parseRed = try $ string "red" >> pure Red

parseGreen = try $ string "green" >> pure Green

parseBlue = try $ string "blue" >> pure Blue

parseColour :: Parser (Colour, Int)
parseColour = do
  number <- many1 digit
  char ' '
  colour <- parseRed <|> parseGreen <|> parseBlue
  return (colour, read number)

parseColours :: Parser [(Colour, Int)]
parseColours = sepBy1 parseColour $ string ", "

mergeColour :: Draw -> (Colour, Int) -> Draw
mergeColour d (Red, i) = d {red = i}
mergeColour d (Green, i) = d {green = i}
mergeColour d (Blue, i) = d {blue = i}

parseDraw :: Parser Draw
parseDraw = do foldl mergeColour emptyDraw <$> parseColours

parseDraws :: Parser [Draw]
parseDraws = sepBy1 parseDraw $ string "; "

{-# ANN parseGame "HLint: ignore Use <$>" #-}
parseGame :: Parser Game
parseGame = do
  string "Game "
  gameId <- many1 digit
  string ": "
  draws <- parseDraws
  return $ Game (read gameId) draws

parseGames :: Parser [Game]
parseGames = sepBy1 parseGame $ char '\n'

-------------
-- PART 01
-------------

isPossible :: Game -> Bool
isPossible (Game _ draws) = all isPossibleDraw draws
  where
    isPossibleDraw (Draw r g b) = r <= 12 && g <= 13 && b <= 14

getIdOfPossibleGame :: Game -> Int
getIdOfPossibleGame g@(Game id draws)
  | isPossible g = id
  | otherwise = 0

day02part01 :: [Game] -> Int
day02part01 games = sum $ map getIdOfPossibleGame games

-------------
-- PART 02
-------------

getMinimumCubes :: Game -> Int
getMinimumCubes (Game id draws) = product [maximum [getByColour d | d <- draws] | getByColour <- [red, green, blue]]

day02part02 :: [Game] -> Int
day02part02 games = sum $ map getMinimumCubes games

-------------
-- MAIN
-------------

main = runWithParser parseGames day02part02 "puzzle/02.txt"
