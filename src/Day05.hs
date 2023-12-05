import Text.Parsec (endOfLine)
import Text.ParserCombinators.Parsec
import Utils (runWithParser)

parseSeeds :: Parser [Int]
parseSeeds = do
  _ <- string "seeds: "
  numbers <- sepBy1 (many1 digit) (char ' ')
  return $ map read numbers

type DstStart = Int

type SrcStart = Int

type RangeLen = Int

type MapLine = (DstStart, SrcStart, RangeLen)

type Mapping = [MapLine]

type MappedNum = Int

type Almanac = ([Int], [Mapping])

parseMapLine :: Parser MapLine
parseMapLine = do
  a <- many1 digit
  _ <- space
  b <- many1 digit
  _ <- space
  c <- many1 digit
  _ <- endOfLine
  return (read a, read b, read c)

parseMapping :: Parser Mapping
parseMapping = do
  _ <- many1 (lower <|> oneOf "-: ")
  _ <- endOfLine
  many1 parseMapLine

parseMappings :: Parser [Mapping]
parseMappings = do
  sepBy1 parseMapping endOfLine

parseAlmanac :: Parser ([Int], [Mapping])
parseAlmanac = do
  seeds <- parseSeeds
  _ <- many1 endOfLine
  mappings <- parseMappings
  return (seeds, mappings)

runMapLine :: Either Int MappedNum -> MapLine -> Either Int MappedNum
runMapLine (Right x) _ = Right x
runMapLine (Left n) (d, s, r)
  | isInRange = Right (d + n - s)
  | otherwise = Left n
  where
    isInRange = n >= s && n < (s + r)

extractNum :: Either Int MappedNum -> Int
extractNum (Left x) = x
extractNum (Right y) = y

runMap :: Mapping -> Int -> Int
runMap m x =
  let startNum = Left x
   in extractNum $ foldl runMapLine startNum m

composeMaps :: [Mapping] -> (Int -> Int)
composeMaps ms = foldl1 (.) $ reverse [runMap m | m <- ms]

-- mapA = [(50, 98, 2), (52, 50, 48)] :: Mapping

-- main = print $ [runMap mapA x | x <- [79, 14, 55, 13]]

day05part1 :: Almanac -> Int
day05part1 (seeds, maps) = minimum [composedMap seed | seed <- seeds]
  where
    composedMap = composeMaps maps

revertMapLine :: MapLine -> MapLine
revertMapLine (d, s, r) = (s, d, r)

revertMaps :: [Mapping] -> [Mapping]
revertMaps ms = reverse [map revertMapLine m | m <- ms]

seedRanges :: [Int] -> [(Int, Int)]
seedRanges [] = []
seedRanges (x : y : xs) = (x, y) : seedRanges xs
seedRanges _ = error "fail to parse seed ranges"

seedInRanges :: [(Int, Int)] -> Int -> Bool
seedInRanges ranges seed = any inRange ranges
  where
    inRange (start, len) = seed >= start && (seed < start + len)

-- rngs = [79, 14, 55, 13]
-- a = seedInRanges (seedRanges rngs) 55
-- main = do
--   print a

day05part2 (seeds, maps) = head [location | location <- [0 ..], let seed = revertedMap location, seedInRanges' seed]
  where
    revertedMap = composeMaps $ revertMaps maps
    seedInRanges' = seedInRanges $ seedRanges seeds

-- main :: IO ()
main = runWithParser parseAlmanac day05part2 "puzzle/05.txt"
