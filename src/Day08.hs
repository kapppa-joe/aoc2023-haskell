import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Utils (runWithParser)

data Direction = L | R deriving (Enum, Show, Read)

type Instruction = [Direction]

type Node = String

type Network = Map.Map Node (Node, Node)

type Step = Int

parseNode :: Parser (Node, Node, Node)
parseNode = do
  curr <- count 3 alphaNum
  _ <- string " = ("
  left <- count 3 alphaNum
  _ <- string ", "
  right <- count 3 alphaNum
  _ <- string ")"

  return (curr, left, right)

buildMap :: [(Node, Node, Node)] -> Network
buildMap [] = Map.empty
buildMap (x : xs) =
  let m = buildMap xs
      (curr, left, right) = x
   in Map.insert curr (left, right) m

parseInput :: Parser (Instruction, Network)
parseInput = do
  inst <- many1 $ oneOf "LR"
  skipMany1 endOfLine
  rawNetwork <- sepBy parseNode endOfLine
  return (map readChar inst, buildMap rawNetwork)
  where
    readChar c = read [c] :: Direction

nextNode :: Network -> Node -> Direction -> Node
nextNode m curr dir =
  let (left, right) = fromJust $ Map.lookup curr m
   in case dir of
        L -> left
        R -> right

exploreNetwork :: Network -> Instruction -> Node -> Step -> Node
exploreNetwork m inst curr step =
  let nextDirectionIndex = step `mod` length inst
      dir = inst !! nextDirectionIndex
   in nextNode m curr dir

toIterater :: (a -> Step -> a) -> ((a, Step) -> (a, Step))
toIterater f (curr, step) = (f curr step, step + 1)

day08part1 :: (Instruction, Network) -> (Node, Step)
day08part1 (inst, m) =
  let explore = toIterater $ exploreNetwork m inst
      start = ("AAA", 0) :: (Node, Step)
      stopCondition (node, _) = node == "ZZZ" :: Bool
   in head $ dropWhile (not . stopCondition) $ iterate explore start

getStepEndsWithZ :: Network -> Instruction -> Node -> Step
getStepEndsWithZ m inst startNode =
  let explore = toIterater $ exploreNetwork m inst
      future = iterate explore (startNode, 0) :: [(Node, Step)]
      possibleStops = filter (\(node, _) -> last node == 'Z') future :: [(Node, Step)]
   in head $ map snd possibleStops

day08part2 :: (Instruction, Map.Map [Char] (Node, Node)) -> Step
day08part2 (inst, m) = foldl1 lcm [getStepEndsWithZ m inst node | node <- startNodes]
  where
    startNodes = filter (\x -> last x == 'A') $ Map.keys m

main :: IO ()
main = do
  runWithParser parseInput day08part2 "puzzle/08.txt"
