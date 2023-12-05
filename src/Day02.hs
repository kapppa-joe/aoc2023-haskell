import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

{-# ANN module "HLint: ignore Use <$>" #-}

data Colour = Red | Green | Blue deriving (Eq, Ord, Enum, Show)

data Draw = Draw
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

type GameId = Int

data Game = Game GameId [Draw] deriving (Show)

parseColour :: Parser (Colour, Int)
parseColour = do
  number <- many1 digit
  spaces
  colour <- parseRed <|> parseGreen <|> parseBlue
  return (colour, read number)
  where
    parseRed = try $ string "red" >> pure Red
    parseGreen = try $ string "green" >> pure Green
    parseBlue = try $ string "blue" >> pure Blue

parseColours :: Parser [(Colour, Int)]
parseColours = sepBy1 parseColour $ string ", "

mergeColour :: Draw -> (Colour, Int) -> Draw
mergeColour d (Red, i) = d {red = i}
mergeColour d (Green, i) = d {green = i}
mergeColour d (Blue, i) = d {blue = i}

parseDraw :: Parser Draw
parseDraw = do
  parsedColours <- parseColours
  return $ foldl mergeColour emptyDraw parsedColours
  where
    emptyDraw = Draw 0 0 0

parseDraws :: Parser [Draw]
parseDraws = sepBy1 parseDraw $ string "; "

parseGame :: Parser Game
parseGame = do
  _ <- string "Game "
  gameId <- many1 digit
  _ <- string ": "
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

-- extractIdForPossibleGames :: Game -> Int
-- extractIdForPossibleGames game@(Game gameId _)
--   | isPossible game = gameId
--   | otherwise = 0

day02part01 :: [Game] -> Int
day02part01 games = sum [gameId | game <- games, let (Game gameId _) = game, isPossible game]

-------------
-- PART 02
-------------

getMinimumCubes :: Game -> Int
getMinimumCubes (Game _ draws) = product [maximum [getByColour d | d <- draws] | getByColour <- [red, green, blue]]

day02part02 :: [Game] -> Int
day02part02 games = sum $ map getMinimumCubes games

-------------
-- MAIN
-------------

main :: IO ()
main = runWithParser parseGames day02part02 "puzzle/02.txt"
