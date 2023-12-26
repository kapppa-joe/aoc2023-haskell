{-# LANGUAGE OverloadedRecordDot #-}

import Data.Heap (MaxPrioHeap)
import qualified Data.Heap as Heap
import Data.List (uncons)
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Utils (testWithExample)

type Node = String

type Weight = Int

type Edge = (Node, Weight)

type Graph = Map.Map Node [Edge]

type MaxHeap = MaxPrioHeap Weight Node

data GraphHeap = GraphHeap {heap :: MaxPrioHeap Weight Node, dict :: Map.Map Node Weight}

instance Show GraphHeap where
  show gheap = show gheap.heap

parseGraph :: [String] -> Graph
parseGraph input = Map.fromListWith (++) $ concatMap parseNodes input
  where
    parseNodes :: String -> [(Node, [Edge])]
    parseNodes line = case uncons $ splitOn ": " line of
      Just (node, [nodes]) -> buildEdges node (splitOn " " nodes)
      _ -> error "failed to parse input"
      where
        buildEdges :: Node -> [Node] -> [(Node, [Edge])]
        buildEdges _ [] = []
        buildEdges node (x : xs) = [(node, [(x, 1)]), (x, [(node, 1)])] ++ buildEdges node xs

pop :: GraphHeap -> ((Weight, Node), GraphHeap)
pop gheap = case Heap.view gheap.heap of
  Nothing -> error "heap is empty"
  Just (item, heap') -> discardStale item
    -- (item, updatedHeap)
    where
      discardStale (weight, node) = case Map.lookup node gheap.dict of
        Nothing -> error "something went wrong"
        Just x -> if x == weight then returnItem else popNext
      returnItem = (item, heapAfterPop)
      popNext = pop gheap {heap = heap', dict = gheap.dict}

      dict' = Map.delete (snd item) gheap.dict
      heapAfterPop = gheap {heap = heap', dict = dict'}

insert :: GraphHeap -> (Weight, Node) -> GraphHeap
insert gheap (w, n) = gheap {heap = Heap.insert (w, n) gheap.heap, dict = Map.insert n w gheap.dict}

checkWeight :: GraphHeap -> Node -> Weight
checkWeight gheap node = Map.findWithDefault 0 node gheap.dict

buildHeapFromList :: [(Node, Weight)] -> GraphHeap
buildHeapFromList xs = GraphHeap heap' dict'
  where
    heap' = Heap.fromList $ map swap xs
    dict' = Map.fromList xs

size :: GraphHeap -> Int
size gheap = Heap.size gheap.heap

trial input = "abc"
  where
    graph = parseGraph input
    len = Map.size graph
    (currNode, currEdges) = head $ Map.assocs graph
    a = Set.singleton currNode
    startHeap = buildHeapFromList currEdges

    heapSize = size startHeap
    -- us = Heap.take (heapSize - 1) startHeap
    -- a' = Set.union (Set.fromList $ map snd us) a

    -- shrinkGraphUntilOneLeft =

    combineNodes :: GraphHeap -> Set.Set Node -> (GraphHeap, Set.Set Node)
    combineNodes gheap set = (updatedHeap, set')
      where
        ((_, nodeU), gheap') = pop gheap
        set' = Set.insert nodeU set
        edges = fromJust $ Map.lookup nodeU graph
        updatedEdges = [(weight', node) | (node, weight) <- edges, Set.notMember node set', let weight' = weight + checkWeight gheap' node]
        updatedHeap = foldl insert gheap' updatedEdges

-- heap' = Heap.fromList [(1, "abc")] :: MaxHeap

main = do
  testWithExample "25" trial