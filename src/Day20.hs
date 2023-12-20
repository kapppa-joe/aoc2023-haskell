{-# LANGUAGE OverloadedRecordDot #-}

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Sequence.FastQueue as Q
import qualified Data.Set as Set
import Utils (testWithExample, debug, runSolution)
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence.FastQueue (ViewL((:<)))

data PulseType = High | Low deriving (Eq, Enum, Show)

type Label = String

type From = Label

type To = Label

type Destinations = [To]

type Pulse = (PulseType, From, To)

data Module =  FlipFlop Destinations | Conjunction (Set.Set From) Destinations | Other Destinations deriving (Eq, Show)

type Modules = Map.Map Label Module

type FlipFlopState = Set.Set Label -- store flipflops which are ON

type ConjunctionState = Map.Map Label (Set.Set Label) -- store source module which were High

data State = State {ffstate :: FlipFlopState, cstate :: ConjunctionState, highCount :: Int, lowCount :: Int, buttonCount :: Int} deriving (Eq, Show)

type PulseQueue = Q.FastQueue Pulse

parseModules :: [String] -> Modules
parseModules input = storeSenderForConjunctions $ Map.fromList $ map parseModule input
  where
    parseModule :: String -> (Label, Module)
    parseModule line = case splitOn " -> " line of
      ['%' : label, dest] -> (label, FlipFlop (splitOn ", " dest))
      ['&' : label, dest] -> (label, Conjunction Set.empty (splitOn ", " dest))
      [label, dest] -> (label, Other (splitOn ", " dest))
      _ -> error "failed to parse input"

    -- isConjunction :: Module -> Bool
    -- isConjunction (Conjunction _ _) = True
    -- isConjunction _ = False

    storeSenderForConjunctions :: Modules -> Modules
    storeSenderForConjunctions m = Map.mapWithKey updateConjunction m
      where
        -- allConjunctions = [label | (label, module_) <- Map.toList m, isConjunction  module_]
        allPairs = [(sender, receiver) | (sender, module_) <- Map.toList m, receiver <- getDestinations module_]
        reverseMap = Map.fromListWith Set.union [(receiver, Set.singleton sender)| (sender, receiver) <- allPairs]

        updateConjunction :: Label -> Module -> Module
        updateConjunction label (Conjunction _ dest) = Conjunction senders dest
          where senders = fromJust $ Map.lookup label reverseMap
        updateConjunction _ module_ = module_



getDestinations :: Module -> Destinations
getDestinations (FlipFlop dests) = dests
getDestinations (Other dests) = dests
getDestinations (Conjunction _ dests) = dests

toggle :: Set.Set Label -> Label -> Set.Set Label
toggle set label =
  if Set.member label set
    then Set.delete label set
    else Set.insert label set

updateMemory :: Set.Set Label -> Label -> PulseType -> Set.Set Label
updateMemory mem sender pulseType = case pulseType of
    High -> Set.insert sender mem
    Low -> Set.delete sender mem

processPulse :: Modules -> State -> Pulse -> (State, [Pulse])
processPulse m st (pulseType, sender, curr) =
  case Map.lookup curr m of
    Nothing -> (countedPulse, []) -- `debug` ("output received pulse: " ++ show pulseType)
    -- Nothing ->  error ("fail to find destination for pulse" ++ show curr)
    Just (Other dests) -> (countedPulse, [(pulseType, curr, dest) | dest <- dests])
    Just (FlipFlop dests) -> handleFF pulseType dests
    Just (Conjunction srcs dests) -> handleConj srcs dests
  where
    countedPulse = if pulseType == High 
      then st {highCount = st.highCount + 1} 
      else st {lowCount = st.lowCount + 1}

    handleFF :: PulseType -> Destinations -> (State, [Pulse])
    handleFF High _ = (countedPulse, [])
    handleFF Low dests = (newState, [(outputPulse, curr, dest) | dest <- dests])
      where
        isOn = Set.member curr st.ffstate
        newFFState = toggle st.ffstate curr
        newState = countedPulse {ffstate = newFFState}
        outputPulse = if isOn then Low else High

    handleConj :: Set.Set Label -> Destinations -> (State, [Pulse])
    handleConj srcs dests = (newState, [(outputPulse, curr, dest) | dest <- dests])
      where
        memory = fromMaybe Set.empty $ Map.lookup curr st.cstate
        updatedMemory = updateMemory memory sender pulseType
        newCState = Map.insert curr updatedMemory st.cstate
        newState = countedPulse {cstate = newCState}
        allHigh = Set.size updatedMemory == Set.size srcs
        outputPulse = if allHigh then Low else High

processButton :: Modules -> State -> State
processButton m st = processQueue buttonCounted initPulseQueue
  where 
    buttonCounted = st { buttonCount = st.buttonCount + 1}
    initPulseQueue :: PulseQueue
    initPulseQueue = Q.fromList [(Low, "button", "broadcaster")]

    processQueue :: State -> PulseQueue -> State
    processQueue st' queue = case Q.viewl queue of
      Q.EmptyL -> st'
      (pulse :< rest) -> processQueue newState updatedQueue
        where 
          (newState, newPulses) = processPulse m st' pulse
          updatedQueue = foldl (Q.|>) rest newPulses

initState :: State 
initState = State Set.empty Map.empty 0 0 0


day20Part1 :: [String] -> Int
day20Part1 input = finalState.lowCount * finalState.highCount
  where
    modules = parseModules input
    processButton' = processButton modules
    iter = iterate processButton' initState
    finalState = iter !! 1000


main :: IO ()
main = do
  runSolution 20 day20Part1
  -- testWithExample "20b" trial