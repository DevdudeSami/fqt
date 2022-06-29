# fqt

**F**unctional **Q**uantum **T**oolkit

FQT is a toolchain for specifying and compiling quantum circuits for FPGA-based customised quantum simulation architectures.


### Disclaimer

This is currently a **work in progress** and is part of my PhD at the University of Glasgow researching FPGA-based simulators for quantum computing circuits.

# Intro

## Core

The modules in `core` provide the eDSL functionality. Primary definitions used to describe quantum circuits specified over an integer-indexed set of qubits are in `core/Definitions.hs`. On top of the `Circuit` type defined there, `core/QCEDSL.hs` provides functionality for specifying quantum circuits over a set of named qubits with higher level functionality like loops and tiling available.

`core/QWMEngine.hs` provides functionality for simulating a general universal quantum circuit specified in the `Circuit` format above. This simulator is implemented in pure Haskell and has no guarantees about performace. This is provided only as a convenience for running toy circuits. `core/LogicQCSimulator.hs` defines a simulator for quantum circuits containing only X gates with any number of controls; i.e. a reversible (or TOFFOLI) circuit simulator. This simulator is useful for debugging quantum circuits which operate only in the computational basis, where only one state is set at any point in the circuit with maximum probability. 

## Compiler

The modules in `compiler` provide compilation functionality for compiling to a an FPGA-specific "quantum problem" formulation. More details will be provided in the future along with the FPGA architecture details.

## Circuits

Example quantum circuit implementations are provided in the `circuits` modules.

## Testing

Some initial functionality for unit testing computational-basis circuits is providing in the `test` modules. `Spec.hs` defines the main entrypoint running for tests. Some tests are provided in `test/QCTestSuites.hs` for QCMaths functions.

Unit tests on circuits are expressed as a set of `Preparation`s and `Expectation`s where a preparation or an expectation is simply a matching of some named qubits to a set of values.

The following is an example for writing a test for a quantum square circuit:

```haskell
square3TestSuite :: QCTestSuite
square3TestSuite = let
	uReg = makeQRegOverID 3 "u"
	rReg@[r0, r1, r2, r3, r4, r5] = makeQRegOverID 6 "r"
	resultRegArrangement = [r4, r5, r0, r1, r2, r3]
	c = "c"
	anc = "anc"
	in QCTestSuite "3-Bit Square Circuit Test Suite" (square uReg rReg c anc) (uReg ++ rReg ++ [c] ++ [anc]) [
		QCTest "3-Bit Square Test: 0^2 = 0"
			[(reverse uReg, "000")] 
			[(reverse resultRegArrangement, "000000")],
		QCTest "3-Bit Square Test: 1^2 = 1"
			[(reverse uReg, "001")]
			[(reverse resultRegArrangement, "000001")],
		QCTest "3-Bit Square Test: 2^2 = 4"
			[(reverse uReg, "010")]
			[(reverse resultRegArrangement, "000100")],
		QCTest "3-Bit Square Test: 3^2 = 9"
			[(reverse uReg, "011")]
			[(reverse resultRegArrangement, "001001")],
		QCTest "3-Bit Square Test: 4^2 = 16"
			[(reverse uReg, "100")]
			[(reverse resultRegArrangement, "010000")],
		QCTest "3-Bit Square Test: 5^2 = 25"
			[(reverse uReg, "101")]
			[(reverse resultRegArrangement, "011001")],
		QCTest "3-Bit Square Test: 6^2 = 36"
			[(reverse uReg, "110")]
			[(reverse resultRegArrangement, "100100")],
		QCTest "3-Bit Square Test: 7^2 = 49"
			[(reverse uReg, "111")]
			[(reverse resultRegArrangement, "110001")]
	]
```

and of course the expectations and preparations can generated automatically "classically":

```haskell
square4TestSuite :: QCTestSuite
square4TestSuite = let
	uReg = makeQRegOverID 4 "u"
	rReg = makeQRegOverID 8 "r"
	resultRegArrangement = rotate (4+1) rReg
	c = "c"
	anc = "anc"
	in QCTestSuite "4-Bit Square Circuit Test Suite" (square uReg rReg c anc) (uReg ++ rReg ++ [c] ++ [anc]) [
		QCTest ("4-Bit Square Test: " ++ (show u) ++ "^2 = " ++ (show (u^2)))
		[(reverse uReg, paddedIntToBitString 4 u)]
		[(reverse resultRegArrangement, paddedIntToBitString 8 (u^2))]
		| u <- [0..15]
		]
```

# Setup

- This repo is a [Stack](https://docs.haskellstack.org/en/stable/README/) project. 
- To get started clone the repo and run `stack build`. 
- To run the tests, run `stack test`.
- To run the main file: `stack run`.

# Using the eDSL

The eDSL relies on a simple fact: A quantum circuit is a list of quantum gate applications. Thus constructing them is as easy as writing lists of quantum gates and concatenating them. We can define any high-level operator as long as it returns a list of gate applications. We then immediately benefit from all the functionality provided by the Haskell language to facilitate classical/quantum co-design. The basic set of gates are defined in `core/QCEDSL.hs`.

Examples of simple quantum circuits are provided here:

```haskell
qft :: NQuGate
qft qReg = let n = length qReg in 
	concat $ concat [h (qReg!!i) : [control (qReg!!j) (rm (j-i+1) (qReg!!i)) | j <- [i+1..n-1]] | i <- [0..n-1]]
```

```haskell
fullAdd :: QReg -> QReg -> Qu -> Qu -> Circ 
fullAdd in1 in2 c z = if length in1 /= length in2 then error "fullAdd: Input qubit register lengths must be identical." else let
	combinedRegister = c : interleave in2 in1
	in 
		ladderQC 2 3 maj combinedRegister ++
		cnot (last in1) z ++
		reverseLadderQC 2 3 unmaj combinedRegister
```
 
# Using the built-in simulator

The following is an example of using the built-in simulator to simulate a quantum circuit:

```haskell
import QCEDSL
import QFT
import QWMEngine

...

qReg = makeQRegOverID 5 "q"
circ = qft qReg
indexedCirc = reduceToIndexedCircuit (circ .~ qReg) 
outputState = preprocessAndRunCircuitOnZeroState indexedCirc
```

Note: The right side of the `.~` specifies the order of the qubits when converting to an indexed circuit for the purpose of QWM-based simulation.

The following is an example of using the logic-based simulator to run a 32-bit full adder:

```haskell
import QCEDSL
import LogicQCSimulator
import Helpers
import QCTesting
import QCMath

...

let 
	aReg = makeQRegOverID 32 "a" -- 32-qubit register
	bReg = makeQRegOverID 32 "b" -- 32-qubit register
	c = "c"
	z = "z" 
	fullReg = aReg ++ bReg ++ [c, z]
	adderCirc = fullAdd aReg bReg c z
	outputBitRegister = prepAndRunLogicQC [
			(reverse aReg, paddedIntToBitString 32 20), -- prepare a as 20
			(reverse bReg, paddedIntToBitString 32 22)  -- prepare b as 22
		] adderCirc fullReg
	outputB = readSubregister fullReg (reverse bReg) outputBitRegister
print adderCirc
print $ bitRegisterToString outputB -- 00000000000000000000000000101010
print $ bitStringToInt $ bitRegisterToString outputB -- 42
```