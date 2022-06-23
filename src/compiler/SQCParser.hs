{-|
Module      : SQCParser
Description : Used for parsing Simple Quantum Circuit representation into the "Circuit" AST.
-}
module SQCParser (
  parseSQC,
  parseSQCFile
) where

import Definitions
import Data.List.Split
import Data.List
import Data.Char

import Debug.Trace

import System.IO
import Control.Monad


-- | Removes white space from input string.
trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Returns a quantum "Gate" from an input string, if possible.
gateFromString :: String -> Maybe Gate
gateFromString "h" = Just H
gateFromString "x" = Just X
gateFromString "y" = Just Y
gateFromString "z" = Just Z
gateFromString "s" = Just S
gateFromString s
  | head s == 'r' = Just $ Rm (read (tail s) :: Int)
  | otherwise = Nothing

-- | The Simple Quantum Circuit representation language.
type SQC = String
-- | A symbol table for qubits in a circuit in SQC.
type QubitSymbols = [String]

-- | Parses a gate and its target from a list of tokens, if possible.
parseGate :: [String] -> QubitSymbols -> (Maybe Gate, Maybe QubitID)
parseGate [g, t] qubitSymbols = (gateFromString g, elemIndex t qubitSymbols)
parseGate s _ = (Nothing, Nothing)

-- | Parses a controlled gate from a list of tokens. Recursively calls "parseGate" until successful to find all the controls.
parseControlledGate :: [String] -> [Control] -> QubitSymbols -> GateApplication
parseControlledGate (c:rest) cs qubitSymbols = case elemIndex c qubitSymbols of
  Just controlIndex -> case parseGate rest qubitSymbols of
    (Just g, Just t) -> mkGateApplication g t (controlIndex:cs)
    (Nothing, _) -> parseControlledGate rest (controlIndex:cs) qubitSymbols
  Nothing -> error "Undefined control qubit"

-- | Parses a controlled gate from a list of tokens. Recursively calls "parseGate" until successful to find all the controls.
-- parseNegControlledGate :: [String] -> [Control] -> [Control] -> QubitSymbols -> GateApplication
-- parseNegControlledGate (c:rest) negControls controls qubitSymbols = case elemIndex c qubitSymbols of
--   Just controlIndex -> case parseGate rest qubitSymbols of
--     (Just g, Just t) -> mkGateApplication (NegCon g (controlIndex:negControls)) t controls
--     (Nothing, _) -> parseNegControlledGate rest (controlIndex:negControls) controls qubitSymbols
--   Nothing -> error "Undefined control qubit"

-- | Parses input SQC into the "Circuit" AST.
parseSQC :: SQC -> Circuit
parseSQC s = let
  lines = splitOn "\n" (trim s)
  tokenisedIntroLine = splitOn " " (trim $ head lines)
  qubitCount = read (head tokenisedIntroLine) :: RegisterSize
  qubitSymbols = tail tokenisedIntroLine :: QubitSymbols
  in Circuit qubitCount [ 
    let tokens = splitOn " " (trim line) in
      if head tokens == "c" then parseControlledGate (tail tokens) [] qubitSymbols else
          case parseGate tokens qubitSymbols of
            (Just g, Just t) -> mkGateApplication g t []
            (Nothing, _) -> error "Bad gate"
          | line <- tail lines ]

parseSQCFile :: String -> IO Circuit
parseSQCFile fileName = do
  src <- openFile fileName ReadMode
  srcContents <- hGetContents src
  let
    circuit = parseSQC srcContents
  return circuit