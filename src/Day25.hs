import Data.Function (on)
import Data.List (maximumBy, uncons)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils (runSolution)

type Node = String

type Edges = Set.Set Node

type Graph = Map.Map Node Edges

type Group = Set.Set Node

parseGraph :: [String] -> Graph
parseGraph input = Map.fromListWith Set.union $ concatMap parseNodes input
  where
    parseNodes :: String -> [(Node, Edges)]
    parseNodes line = case uncons $ splitOn ": " line of
      Just (node, [nodes]) -> buildEdges node (splitOn " " nodes)
      _ -> error "failed to parse input"
      where
        buildEdges :: Node -> [Node] -> [(Node, Edges)]
        buildEdges _ [] = []
        buildEdges node (x : xs) = [(node, Set.singleton x), (x, Set.singleton node)] ++ buildEdges node xs

day25 :: [String] -> (Int, Int, Int)
day25 input = (Set.size groupA * Set.size groupB, Set.size groupA, Set.size groupB)
  where
    graph = parseGraph input
    initSet = Set.fromList $ Map.keys graph
    iter = iterate removeStranger initSet

    groupA = head $ dropWhile (not . separatedByThreeCuts) iter
    groupB = Set.difference initSet groupA

    removeStranger :: Group -> Group
    removeStranger group = Set.delete strangerNode group
      where
        strangerNode = maximumBy (compare `on` count') $ Set.toList group
        count' = countExternalNeighbour group

    separatedByThreeCuts :: Group -> Bool
    separatedByThreeCuts group = externalConnections == 3
      where
        externalConnections = sum $ map count' $ Set.toList group
        count' = countExternalNeighbour group

    countExternalNeighbour :: Group -> Node -> Int
    countExternalNeighbour group node = Set.size neighboursNotInThisGroup
      where
        allNeighbours = Map.findWithDefault Set.empty node graph
        neighboursNotInThisGroup = Set.difference allNeighbours group

main :: IO ()
main = do
  runSolution 25 day25
