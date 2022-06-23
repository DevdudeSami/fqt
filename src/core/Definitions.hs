{-|
Module      : Definitions
Description : Defines types commonly used throughout the toolchain.
-}
module Definitions (
  QubitID, Control, RegisterSize, AmplitudeIndex, State, GateApplicationIteration, Amplitude, Gate(..), GateApplication(..), Circuit(..), Substate(..), GateMatrix(..), Probability, (~+), mkGateApplication, showState, 
) where

import Data.Complex
import Data.List (intercalate)
import Helpers (paddedIntToBitString)
import Math.NumberTheory.Logarithms

-- | A qubit index in a register.
type QubitID = Int
-- | Index of a qubit to be used as control.
type Control = QubitID
-- | Number of qubits in a quantum register.
type RegisterSize = Int
-- | Index of an amplitude in a state vector.
type AmplitudeIndex = Int
-- | A quantum register.
type State = [Amplitude]
-- | Pair iteration index for running a quantum gate.
type GateApplicationIteration = Int
-- | A quantum amplitude.
type Amplitude = Complex Float
-- | An element in the matrix of a quantum gates.
type MatElement = Complex Float
-- | Probability of getting some specific state after a quantum measurement.
type Probability = Float

-- | Supported single-qubit gates with arbitrary number of controls.
data Gate = H | X | Y | Z | S | S' | R Float | Rm Int | Swap QubitID | NegCon Gate [Control] | UnconX deriving (Show, Eq)

-- | Represents a quantum gate to be applied to a single qubit with an arbitrary number of controls at a specific pair iteration index.
data GateApplication = GateApplication {
  gate :: Gate,
  target :: QubitID,
  controls :: [Control],
  iteration :: GateApplicationIteration
} deriving (Eq)

instance Show GateApplication where
  show (GateApplication gate target controls iteration) = show gate ++ " " ++ show target ++ " " ++ show controls

-- | Makes an initial GateApplication (with pair iteration index 0).
mkGateApplication :: Gate -> QubitID -> [Control] -> GateApplication
mkGateApplication g t cs = GateApplication g t cs 0

-- TODO: change to pad with zeroes on the left
showState :: State -> String
showState s = let
  nQubits = integerLog2 $ toInteger (length s)
  go :: String -> State -> Int -> String
  go res [] _ = res ++ "\n]"
  go res (a:as) i = go (res ++ "\n\t" ++ paddedIntToBitString nQubits i ++ "\t" ++ show a) as (i+1)
  in go "[" s 0

-- | Represents a quantum circuit with some number of qubits and a list of GateApplications.
data Circuit = Circuit {
  -- | Number of qubits in the register.
  n :: RegisterSize,
  gateApplications :: [GateApplication]
}

instance Show Circuit where
  show (Circuit n gs) = "Circuit with " ++ show n ++ " qubits:\n\t" ++ intercalate "\n\t" (map show gs)

infixl 7 ~+
-- | Appends gate applications to an existing circuit, returning a new one.
(~+) :: Circuit -- ^ Initial circuit.
     -> [GateApplication] -- ^ GateApplications to add.
     -> Circuit -- ^ Resulting circuit
(~+) (Circuit n gas0) gas1 = Circuit n (gas0++gas1)

-- | An excerpt pair of amplitudes from a quantum state to be used in a pair iteration during a gate application.
data Substate = Substate Amplitude Amplitude deriving Show

-- | A matrix representing a single-qubit quantum gate.
data GateMatrix = GateMatrix MatElement MatElement MatElement MatElement deriving Show
