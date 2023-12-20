{-# LANGUAGE OverloadedRecordDot #-}

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Sequence.FastQueue as Q
import qualified Data.Set as Set
import Utils (testWithExample)

data PulseType = High | Low deriving (Eq, Enum, Show)

data ModuleType = Broadcaster | FlipFlop | Conjunction deriving (Eq, Enum, Show)

type Label = String

type From = Label

type To = Label

type Pulse = (PulseType, From, To)

type Destinations = [To]

type Modules = Map.Map Label (ModuleType, Destinations)

type FlipFlopState = Set.Set Label -- store flipflops which are ON

type ConjunctionState = Map.Map Label (Map.Map Label Bool)

data State = State {ffstate :: FlipFlopState, cstate :: ConjunctionState, highCount :: Int, lowCount :: Int}

type PulseQueue = Q.FastQueue Pulse

parseModules :: [String] -> Modules
parseModules input = Map.fromList $ map parseModule input
  where
    parseModule :: String -> (Label, (ModuleType, Destinations))
    parseModule line = case splitOn " -> " line of
      ["broadcaster", dest] -> ("broadcaster", (Broadcaster, splitOn ", " dest))
      ['%' : xs, dest] -> (xs, (FlipFlop, splitOn ", " dest))
      ['&' : xs, dest] -> (xs, (Conjunction, splitOn ", " dest))
      _ -> error "failed to parse input"

toggle :: FlipFlopState -> Label -> FlipFlopState
toggle ffstate label =
  if Set.member label ffstate
    then Set.delete label ffstate
    else Set.insert label ffstate

processPulse :: Modules -> State -> Pulse -> (State, [Pulse])
processPulse m st (pulseType, from, curr) =
  case Map.lookup curr m of
    Nothing -> error "fail to find destination for pulse"
    Just (Broadcaster, dests) -> (st, [(pulseType, curr, dest) | dest <- dests])
    Just (FlipFlop, dests) -> handleFF pulseType dests
    Just (Conjunction, dests) -> handleConj
  where
    handleFF :: PulseType -> Destinations -> (State, [Pulse])
    handleFF High _ = (st, [])
    handleFF Low dests = (newState, [(outputPulse, curr, dest) | dest <- dests])
      where
        isOn = Set.member curr st.ffstate
        newFFState = toggle st.ffstate curr
        newState = st {ffstate = newFFState}
        outputPulse = if isOn then Low else High

    handleConj :: (State, [Pulse])
    handleConj = (st, [])
      where
        memory = Map.lookup curr st.cstate
        


main :: IO ()
main = do
  testWithExample "20" parseModules