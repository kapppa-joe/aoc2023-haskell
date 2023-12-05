import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

---------------
-- Parsing
---------------

type DstStart = Int

type SrcStart = Int

type RangeLen = Int

type PartialMap = (DstStart, SrcStart, RangeLen)

type Mapping = [PartialMap]

type MappedNum = Int

type Almanac = ([Int], [Mapping])

parseSeeds :: Parser [Int]
parseSeeds = do
  _ <- string "seeds: "
  numbers <- sepBy1 (many1 digit) (char ' ')
  return $ map read numbers

parsePartialMap :: Parser PartialMap
parsePartialMap = do
  a <- many1 digit
  _ <- space
  b <- many1 digit
  _ <- space
  c <- many1 digit
  _ <- endOfLine
  return (read a, read b, read c)

parseMappings :: Parser [Mapping]
parseMappings = do
  sepBy1 parseMapping endOfLine
  where
    parseMapping = do
      _ <- many1 (lower <|> oneOf "-: ")
      _ <- endOfLine
      many1 parsePartialMap

parseAlmanac :: Parser ([Int], [Mapping])
parseAlmanac = do
  seeds <- parseSeeds
  _ <- many1 endOfLine
  mappings <- parseMappings
  return (seeds, mappings)

---------------
-- Part 1
---------------

runPartialMap :: Either Int MappedNum -> PartialMap -> Either Int MappedNum
runPartialMap (Right x) _ = Right x
runPartialMap (Left n) (d, s, r)
  | isInRange = Right (d + n - s)
  | otherwise = Left n
  where
    isInRange = n >= s && n < (s + r)

runMap :: Mapping -> Int -> Int
runMap m x = extractNum $ foldl runPartialMap startNum m
  where
    startNum = Left x
    extractNum wrappedNum = case wrappedNum of
      Left a -> a
      Right a -> a

composeMaps :: [Mapping] -> (Int -> Int)
composeMaps ms = foldl1 (.) $ reverse [runMap m | m <- ms]

day05part1 :: Almanac -> Int
day05part1 (seeds, maps) = minimum [composedMap seed | seed <- seeds]
  where
    composedMap = composeMaps maps

---------------
-- Part 2
---------------
type Start = Int

type End = Int

type Range = (Start, End) -- right exclusive, i.e. (1,4) -> [1, 2, 3]

type Mapped = [Range]

type NotMapped = [Range]

type SeedRangesInProcess = (NotMapped, Mapped)

seedRanges :: [Int] -> [Range]
seedRanges [] = []
seedRanges (x : y : xs) = (x, x + y) : seedRanges xs
seedRanges _ = error "fail to parse seed ranges"

runPartialMapOnRange :: PartialMap -> Range -> SeedRangesInProcess
runPartialMapOnRange (d, s, r) (start, end)
  | overlapStart > overlapEnd = ([(start, end)], [])
  | otherwise = (nonMappedRanges, [mappedRange])
  where
    mapRangeEnd = s + r
    overlapStart = max s start
    overlapEnd = min mapRangeEnd end
    diff = d - s
    mappedRange = (overlapStart + diff, overlapEnd + diff)
    nonMappedRanges = discardNull [(start, overlapStart), (overlapEnd, end)]
    discardNull = filter (\(a, b) -> b > a) :: [Range] -> [Range]

runPartialMapOnMultiRanges :: SeedRangesInProcess -> PartialMap -> SeedRangesInProcess
runPartialMapOnMultiRanges (notMapped, mapped) m =
  let (notMapped', mapped') = unzip [runPartialMapOnRange m range | range <- notMapped]
   in (foldl1 (++) notMapped', foldl1 (++) mapped' ++ mapped)

mapRanges :: Mapping -> [Range] -> [Range]
mapRanges m r =
  let initialRanges = (r, []) :: SeedRangesInProcess
      (notMapped, mapped) = foldl runPartialMapOnMultiRanges initialRanges m
   in notMapped ++ mapped

composeMaps' :: [Mapping] -> ([Range] -> [Range])
composeMaps' maps = foldl1 (.) $ reverse [mapRanges m | m <- maps]

day05part2 :: Almanac -> Int
day05part2 (seeds, maps) = minimum [fst rng | rng <- locationRanges]
  where
    initalSeedRanges = seedRanges seeds
    composedMap = composeMaps' maps
    locationRanges = composedMap initalSeedRanges

main :: IO ()
main = runWithParser parseAlmanac day05part2 "puzzle/05.txt"
