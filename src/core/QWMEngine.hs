{-|
Module      : QWMEngine
Description : Local CPU Quantum Circuit simulation engine. To be used for verifying small circuits.
-}
module QWMEngine (
  runCircuit,
  runCircuitOnZeroState,
  replace, stateIndices,
  chainCircuit,
  findOneResult,
  inverse,
  circuitInverse,
  preprocessGate,
  preprocessCircuit,
  preprocessAndRunCircuit,
  preprocessAndRunCircuitOnZeroState,
  stateMagnitude
) where

import Definitions
    ( Circuit(Circuit, n),
      GateApplication(GateApplication),
      Gate(..),
      State,
      QubitID,
      GateMatrix(..),
      Substate(..),
      RegisterSize,
      AmplitudeIndex,
      GateApplicationIteration,
      Control,
      Probability, mkGateApplication, Amplitude )
import Data.Complex
import Data.Array.IO
import Data.Bits
import Debug.Trace ( traceShow )

-- | Replaces a value in a list at a specific index, returning the new list.
replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- | Returns the indices of the pair of amplitudes to be used a substate for a given "GateApplicationIteration" index and a target "QubitID". This is the equivalent of the nthCleared function in other language implementations.
stateIndices :: GateApplicationIteration  -- ^ Iteration index in gate execution.
  -> QubitID   -- ^ Target qubit index.
  -> (AmplitudeIndex, AmplitudeIndex)
stateIndices i t = let
    mask = 1 `shiftL` t - 1
    notMask = complement mask
    firstIndex = i .&. mask .|. (i .&. notMask) `shiftL` 1
    secondIndex = firstIndex .|. 1 `shiftL` t
    in (firstIndex, secondIndex)

-- | Multiplies a 2x2 "GateMatrix" by a given 2-dimensional "Substate", returning the resulting 2-dimensional "Substate".
applyGateMatrix :: GateMatrix -> Substate -> Substate
applyGateMatrix (GateMatrix g0 g1 g2 g3) (Substate s0 s1) = let
  s0' = g0 * s0 + g1 * s1
  s1' = g2 * s0 + g3 * s1
  in Substate s0' s1'

-- | Applies a 2x2 "GateMatrix" to a given 2-dimensional "Substate" with some control flags resulting from processing the control qubits on the gate iteration, returning the resulting 2-dimensional "Substate".
applyControlledGateMatrix :: GateMatrix -- ^ 2x2 matrix to apply.
  -> (Bool, Bool) -- ^ Control flags; an amplitude is only updated if its respective flag is true.
  -> Substate -- ^ Input 2-dimensional state vector.
  -> Substate -- ^ Output 2-dimensional state vector.
applyControlledGateMatrix (GateMatrix g0 g1 g2 g3) (cFlag0, cFlag1) (Substate s0 s1) = let
  s0' = if cFlag0 then g0 * s0 + g1 * s1 else s0
  s1' = if cFlag1 then g2 * s0 + g3 * s1 else s1
  in Substate s0' s1'

-- | Returns the 2x2 "GateMatrix" representation of the given "Gate".
gateMatrixFromGate :: Gate -> GateMatrix
gateMatrixFromGate H = GateMatrix (1/sqrt 2) (1/sqrt 2) (1/sqrt 2) ((-1)/sqrt 2)
gateMatrixFromGate X = GateMatrix 0 1 1 0
gateMatrixFromGate Y = GateMatrix 0 (0:+(-1)) (0:+1) 0
gateMatrixFromGate Z = GateMatrix 1 0 0 (-1)
gateMatrixFromGate S = GateMatrix 1 0 0 (0:+1)
gateMatrixFromGate S' = GateMatrix 1 0 0 (0:+(-1))
gateMatrixFromGate (R theta) = GateMatrix 1 0 0 (exp (0:+theta))
gateMatrixFromGate (Rm m) = gateMatrixFromGate $ R (if m>0 then 2*pi/(2^m) else (-2*pi/(2^(-m))))

-- | Returns the inverse of a gate.
inverse :: Gate -> Gate
inverse H = H
inverse X = X
inverse Y = Y
inverse Z = Z
inverse S = S'
inverse S' = S
inverse (R theta) = R (-theta)
inverse (Rm m) = Rm (-m)
inverse (Swap t) = Swap t
inverse UnconX = UnconX

preprocessGate :: GateApplication -> [GateApplication]
preprocessGate (GateApplication (Swap t1) t0 cs i) = concatMap preprocessGate [
  GateApplication X t1 [t0] i,
  GateApplication X t0 [t1] i,
  GateApplication X t1 [t0] i
  ]
preprocessGate (GateApplication (NegCon g negControls) t cs i) = let
  negationGates = map (\c->GateApplication X (abs c) [] i) negControls
  in negationGates ++ preprocessGate (GateApplication g t (negControls++cs) i) ++ negationGates
preprocessGate (GateApplication UnconX t cs i) = [GateApplication X t [] i] -- ignore controls for UnconX
preprocessGate g = [g]

-- | Takes a circuit which may have swaps, negated controls, or other unimplemented functionality and returns a processed circuit with those components destructured to implemented gates.
preprocessCircuit :: Circuit -> Circuit
preprocessCircuit (Circuit n gas) = Circuit n (concatMap preprocessGate gas)

-- | Returns the inverse of a circuit, suitable to be used for uncomputation.
circuitInverse :: Circuit -> Circuit
circuitInverse (Circuit n gas) = let
  revGas = reverse gas
  go :: [GateApplication] -> [GateApplication]
  go [] = []
  go (GateApplication g t cs i:gas) = GateApplication (inverse g) t cs i:go gas
  in Circuit n (go revGas)

-- | Applies a quantum "Gate" to a given 2-dimensional "Substate" with some control flags resulting from processing the control qubits on the gate iteration, returning the resulting 2-dimensional "Substate".
applyGateToSubstate :: Gate -- ^ Quantum "Gate" to apply.
  -> (Bool, Bool) -- ^ Control flags; an amplitude is only updated if its respective flag is true.
  -> Substate -- ^ Input 2-dimensional state vector.
  -> Substate -- ^ Output 2-dimensional state vector.
applyGateToSubstate g (True, True) = applyGateMatrix (gateMatrixFromGate g)
applyGateToSubstate g cFlags = applyControlledGateMatrix (gateMatrixFromGate g) cFlags

-- | Returns the "Substate" for a given "GateApplicationIteration" while processing a quantum gate on the input "State" for some target "QubitID".
substateForGateIteration :: State -> GateApplicationIteration -> QubitID -> Substate
substateForGateIteration s i t = let
  (i0, i1) = stateIndices i t
  in Substate (s!!i0) (s!!i1)

-- TODO: The following function can be implemented more efficiently by doing a single replace that does both element.
-- | Replaces a pair of amplitudes in the input "Circuit" corresponding to a "GateApplicationIteration" on a given target "QubitID" with a given new "Substate", returning the new "State".
replaceStateAtGateIteration :: State -> QubitID -> GateApplicationIteration -> Substate -> State
replaceStateAtGateIteration s t i (Substate a0 a1) = let
  (i0, i1) = stateIndices i t
  in replace i0 a0 (replace i1 a1 s)

-- | Returns a pair of flags that represent whether to apply some "GateApplicationIteration" on a given target "QubitID" for some list of "Control"s.
controlFlags :: QubitID -> [Control] -> GateApplicationIteration -> (Bool, Bool)
controlFlags t cs i = let
  (i0, i1) = stateIndices i t
  amp0ControlFlags = map (\c -> (1 `shiftL` c) .&. i0 > 0) cs
  amp1ControlFlags = map (\c -> (1 `shiftL` c) .&. i1 > 0) cs
  in (and amp0ControlFlags, and amp1ControlFlags)

-- | Applies a prepared "GateApplication" to the input quantum "State", returning the new "State".
applyGate :: State -> GateApplication -> State
applyGate s (GateApplication g t cs i)
  | i == length s `div` 2 = s
  | otherwise = let
  cFlags = controlFlags t cs i
  newSubstate = applyGateToSubstate g cFlags (substateForGateIteration s i t)
  newState = replaceStateAtGateIteration s t i newSubstate
  in applyGate newState (GateApplication g t cs (i+1))

-- | Returns the probabilities of measuring each possible state in an input quantum "State".
probabilities :: State -> [Probability]
probabilities = map (\a -> (magnitude a) ** 2)

-- | Returns a n-qubit state with all qubits set to |0>.
zeroState :: RegisterSize -> State
zeroState n = 1 : [0 | i <- [1..(2^n)-1]]

-- | Applies an input quantum "Circuit" to a given state, returning the new state.
runCircuit :: Circuit -> State -> State
runCircuit (Circuit _ []) s = s
runCircuit (Circuit n (ga:gas)) s = runCircuit (Circuit n gas) (applyGate s ga)

-- | Preprocesses a circuit and applies it to a given state.
preprocessAndRunCircuit :: Circuit -> State -> State
preprocessAndRunCircuit c = runCircuit (preprocessCircuit c)

-- | Applies an input quantum "Circuit" to the zero state |00...0>, returning the resulting state.
runCircuitOnZeroState :: Circuit -> State
runCircuitOnZeroState c = runCircuit c (zeroState $ n c)

-- | Applies an input quantum "Circuit" to the zero state |00...0>, returning the resulting state, after applying preprocessing.
preprocessAndRunCircuitOnZeroState :: Circuit -> State
preprocessAndRunCircuitOnZeroState = runCircuitOnZeroState . preprocessCircuit

findOneResult :: State -> Maybe AmplitudeIndex
findOneResult s = let
  go :: State -> Int -> Maybe AmplitudeIndex
  go _ 0 = Nothing
  go (a:as) i = (if round (realPart a) == 1 then Just ((length s)-i) else go as (i-1))
  in go s (length s)

findNonZeroResults :: State -> [(AmplitudeIndex , Amplitude)]
findNonZeroResults s = let
  go :: State -> Int -> [(AmplitudeIndex , Amplitude)] -> [(AmplitudeIndex , Amplitude)]
  go _ 0 res = res
  go (a:as) i res = if realPart a == 0 && imagPart a == 0 then go as (i-1) res else go as (i-1) ((i,a):res)
  in go s (length s) []

-- | Chains a circuit to a smaller one given some target qubit map (of the size of the smaller circuit register) and some controls in the main circuit (must be exclusive of the qubit map targets).
chainCircuit :: Circuit -- ^ Main (larger) quantum circuit.
  -> Circuit -- ^ Smaller quantum circuit to chain.
  -> [QubitID] -- ^ Target qubit map from the smaller circuit to the larger one. Must map all qubits in the smaller circuit.
  -> [Control] -- ^ Controls in the larger circuit (must be exclusive of the qubit map targets).
  -> [Control] -- ^ Negative controls in the larger circuit (must be exclusive of the qubit map targets).
  -> Circuit -- ^ Output combined circuit.
chainCircuit (Circuit n0 gas0) (Circuit _ []) _ _ _ = Circuit n0 gas0
chainCircuit (Circuit n0 gas0) (Circuit n1 ((GateApplication g t cs _):gas1)) qubitMap newControls newNegativeControls = chainCircuit (Circuit n0 (gas0++[mkGateApplication (NegCon g newNegativeControls) (qubitMap!!t) (newControls ++ map (qubitMap !!) cs)])) (Circuit n1 gas1) qubitMap newControls newNegativeControls

stateMagnitude :: State -> Float
stateMagnitude s = let
  go :: State -> Float -> Float
  go [] acc = acc
  go (a:as) acc = go as (acc + (magnitude a) ** 2)
  in go s 0
