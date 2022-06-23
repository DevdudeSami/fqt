module QCMath (
	maj, unmaj,
	fullAdd, modAdd,
	leftShift, rightShift,
	square, inverseSquare, squareAndShiftBack,
	sgn,
	minusOneOperator, minusTwoOperator, minusThreeOperator
	) where

	import QCEDSL
	import Helpers

	maj :: NQuGate
	maj [q0, q1, q2] = cnot q2 q1 ++ cnot q2 q0 ++ toff q0 q1 q2

	unmaj :: NQuGate 
	unmaj [q0, q1, q2] = toff q0 q1 q2 ++ cnot q2 q0 ++ cnot q0 q1

	-- | Cuccaro adder
	fullAdd :: QReg -> QReg -> Qu -> Qu -> Circ 
	fullAdd in1 in2 c z = if length in1 /= length in2 then error "fullAdd: Input qubit register lengths must be identical." else let
		combinedRegister = c : interleave in2 in1
		in 
			ladderQC 2 3 maj combinedRegister ++
			cnot (last in1) z ++
			reverseLadderQC 2 3 unmaj combinedRegister

	-- | Modulo adder
	modAdd :: QReg -> QReg -> Qu -> Circ
	-- modAdd in1 in2 c = if length in1 /= length in2 then error "modAdd: Input qubit register lengths must be identical." else let
	-- 	smallerAdder = fullAdd (dropLast 1 in1) (dropLast 1 in2) c (last in1)
	-- 	in smallerAdder ++ cnot (last in1) (last in2)
	modAdd in1 in2 c = if length in1 /= length in2 then error "modAdd: Input qubit register lengths must be identical." else let
		combinedRegister = c : interleave in2 in1
		in
			ladderQC 2 3 maj combinedRegister ++
			reverseLadderQC 2 3 unmaj combinedRegister

	swapPyramid :: QReg -> Circ			
	swapPyramid [] = []
	swapPyramid [_] = []
	swapPyramid (q:qs) = swap q (last qs ) ++ swapPyramid (dropLast 1 qs)

	leftShift :: QReg -> Circ 
	leftShift reg = swapPyramid reg ++ swapPyramid (dropLast 1 reg)

	rightShift :: QReg -> Circ
	rightShift reg = swapPyramid reg ++ swapPyramid (drop 1 reg)

	square :: QReg -> QReg -> Qu -> Qu -> Circ
	square u r c anc = if 2*length u /= length r then error "square: The size of register r must be double that of u." else let
		inputSize = length u
		adderCirc = control anc $ fullAdd u (quRange r (head r) (r!!(inputSize-1))) c (r!!inputSize)
		controlledAdderBlock ctrl = cnot ctrl anc ++ adderCirc ++ cnot ctrl anc
		shiftCirc = leftShift r in
			loopSingleQubitGateOverReg (\ctrl -> controlledAdderBlock ctrl ++ shiftCirc) (take (inputSize-1) u) ++
			controlledAdderBlock (last u)

	squareAndShiftBack :: QReg -> QReg -> Qu -> Qu -> Circ
	squareAndShiftBack u r c anc = if 2*length u /= length r then error "squareAndShiftBack: The size of register r must be double that of u." else let
		inputSize = length u
		in 
			square u r c anc ++
			repeatCirc (inputSize-1) (rightShift r)

	inverseSquare :: QReg -> QReg -> Qu -> Qu -> Circ
	inverseSquare u r c anc = if 2*length u /= length r then error "square: The size of register r must be double that of u." else let
		inputSize = length u
		adderCirc = (.!) $ control anc $ fullAdd u (quRange r (head r) (r!!(inputSize-1))) c (r!!inputSize)
		controlledAdderBlock ctrl = cnot ctrl anc ++ adderCirc ++ cnot ctrl anc
		shiftCirc = rightShift r in
			loopSingleQubitGateOverReg (\ctrl -> controlledAdderBlock ctrl ++ shiftCirc) (reverse $ tail u) ++
			controlledAdderBlock (head u)

	sgn :: QReg -> Circ  -- passed in as q0, q1, q2, ..., qn-1 
	sgn reg = let
		block i = nqCNot $ [reg!!j | j <- [0..length reg - 2 - i]] ++ [reg!!(length reg - 1 - i)]
		in 
			loopSingleQubitGateOverReg x reg ++
			concatMap block [0..(length reg - 1)]

	minusOneOperator :: (Qu, Qu, Qu) -> Circ
	minusOneOperator (q0, q1, q2) = 
		x q0 ++
		cnot q0 q1 ++
		toff q0 q1 q2

	minusTwoOperator :: (Qu, Qu, Qu) -> Circ
	minusTwoOperator (q0, q1, q2) = 
		x q2 ++
		cnot q1 q2 ++
		x q1

	minusThreeOperator :: (Qu, Qu, Qu) -> Circ
	minusThreeOperator (q0, q1, q2) = 
		x q2 ++
		negControl q0 (x q1) ++
		toff q0 q1 q2 ++ 
		x q0 ++ x q1
