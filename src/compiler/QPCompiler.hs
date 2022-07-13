{-|
Module      : QPCompiler
Description : Contains functions for compiling "Circuit" ASTs to Quantum Problems (QP) suitable for running on a customised FPGA architecture.
-}
module QPCompiler(
  QProblem,
  compileCircuitToQProblemFile
) where

import System.IO

import Definitions

import Data.List
import Data.List.Utils (replace)

import System.Directory

import SQCParser
import Debug.Trace

import Control.Monad

-- | Represents all the parameters needed to generate OCL files for some circuit from the templates.
data QProblem = QProblem {
  n :: RegisterSize, -- ^ Number of qubits in the register
  stateSize :: Int, -- ^ The value of N = n^2 provided to the templates for convenience.
  gateCount :: Int, -- ^ Total number of gates to be applied.
  stateR :: [Float], -- ^ Real part of the input state.
  stateI :: [Float], -- ^ Imaginary part of the input state
  problem :: [Int], -- ^ List of ints representing the gates. Each gate is 4 number: (numeric gate code, target qubit ID, first control, second control), where the controls can take on the values of the target ID to denote no control.
  maxControls :: Int -- ^ Number of controls for the gate with the most controls.
} deriving Show

-- | Returns the numeric gate code of a given quantum "Gate".
gateCodeFromGate :: Gate -> Int
gateCodeFromGate H = 0
gateCodeFromGate X = 2
gateCodeFromGate Y = 3
gateCodeFromGate Z = 4
gateCodeFromGate S = 5
gateCodeFromGate (Rm m) = 1000 + m

-- | Returns a list of controls padded with a target "QubitID" if necessary. 
padControls :: Int -> QubitID -> [Control] -> [Control]
padControls maxControls t cs = let
  go :: Int -> QubitID -> [Control] -> [Control] -> [Control]
  go maxControls t cs padded
    | length padded == maxControls = padded
    | null cs = go maxControls t cs (t:padded)
    | otherwise = go maxControls t (tail cs) (head cs:padded)
  in go maxControls t cs []

-- | Converts a given quantum "Circuit" to its "QProblem" representation, suitable to be used to process OCL template files, or simply generate a QP file
-- | Takes an Optional maxControls argument, otherwise the maxControls is found from the circuit.
compileCircuitToQProblem :: Circuit -> Maybe Int -> QProblem
compileCircuitToQProblem (Circuit n gas) maxControls = let
  stateSize = 2^n
  gateCount = length gas
  stateR = 1 : [0 | i <- [1..stateSize-1]]
  stateI = [0 | i <- [0..stateSize-1]]
  mC = case maxControls of 
    Just maxControls -> maxControls
    Nothing -> maximum $ map (length . controls) gas
  problem = concatMap (\(GateApplication g t cs _) -> [gateCodeFromGate g, t] ++ padControls mC t cs) gas
  in QProblem n stateSize gateCount stateR stateI problem mC

-- | Compiles the given circuit into a QProblem, suitable for passing into an FPGA host template file of the format:
-- | n problem
-- | where n is the number of qubits, and problem is the QProblem of the form:
-- | (gateCode target control*)*
-- | where there are as many controls as passed into the maxControls arguments. This is because the FPGA design
-- | is static and requires a fixed known number of max controls.
compileCircuitToQProblemFile :: Circuit -> Int -> String -> IO ()
compileCircuitToQProblemFile c maxControls destPath = let
  QProblem n _ gateCount _ _ problem _ = compileCircuitToQProblem c (Just maxControls)
  preparedProblem = unwords (map show (n:problem))
  in do
    writeFile destPath preparedProblem