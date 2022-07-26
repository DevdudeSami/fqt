module QCEDSL (
	Qu, QReg, GateApp, Circ, CircOverQReg(..), GateApplicationOverNamedQubit(..),
	validateCircOverQReg, validateGateAppOverQReg,
	(.~), (.!), (.<), (.==),
	SingleQuGate, DoubleQuGate, TripleQuGate, NQuGate,
	h, x, y, z, rm, cnot, swap, toff, nqCNot, cnotTile,
	control, controls, negControl, negControls, controlQRegPattern, controlTile,
	ifQ, ifQs, ifNotQ, ifNotQs,
	repeatCirc, loop, loopSingleQubitGateOverReg,
	ladderQC, reverseLadderQC,
	rev, gateInv, inv,
	reduceToIndexedCircuit,
	quRange,
	readFirstOneResult, readFirstOneResultOverRegister, readSubregisterResultFromInt,
	mapInputQubitsToInputStatesOverRegister,
	makeQRegOverID
	) where

	import Definitions ( 
		Gate(X, H, Y, Z, Rm, UnconX), 
		Circuit(..), 
		State, 
		QubitID,
		GateApplication, 
		mkGateApplication 
		)
	import Helpers
	import QWMEngine ( inverse, findOneResult )

	import Data.List
	import Data.Maybe ( fromJust )
	
	import Debug.Trace

	type QubitName = String
	type NamedQRegister = [QubitName]

	data GateApplicationOverNamedQubit = GateApplicationOverNamedQubit {
		gate :: Gate,
		targetQubit :: QubitName,
		controlQubits :: [QubitName]
	} deriving (Eq)

	instance Show GateApplicationOverNamedQubit where
		show (GateApplicationOverNamedQubit gate target controls) = show gate ++ " " ++ target ++ " " ++ show controls

	type Qu = QubitName 
	type QReg = NamedQRegister
	type GateApp = GateApplicationOverNamedQubit

	mapNamedQubitToQubitID :: NamedQRegister -> QubitName -> QubitID
	mapNamedQubitToQubitID qubitNames qubitName = 
		case elemIndex qubitName qubitNames of
			Just i -> i
			Nothing -> error $ "mapNamedQubitToQubitID: qubitName " ++ qubitName ++ " not found in qubitNames " ++ show qubitNames

	mapNamedGateApplications :: NamedQRegister -> [GateApplicationOverNamedQubit] -> [GateApplication]
	mapNamedGateApplications qRegister = map (\(GateApplicationOverNamedQubit gate t cs) -> mkGateApplication gate (mapNamedQubitToQubitID qRegister t) (map (mapNamedQubitToQubitID qRegister) cs))
	
	readOneResultFromState :: Int -> State -> String
	readOneResultFromState n state = let
		resIndex = case findOneResult state 
			of Just i -> i
			   Nothing -> traceShow state (error "readOneResultFromState: no result found")
		bitString = reverse $ intToBitString resIndex
		in if length bitString < n then bitString ++ replicate (n - length bitString) '0' else bitString

	readOneResultFromStateOverRegister :: NamedQRegister -> NamedQRegister -> State -> String
	readOneResultFromStateOverRegister qReg reqQReg state = let
		resBitString = readOneResultFromState (length qReg) state
		in concatMap (\q -> [resBitString !! fromJust (q `elemIndex` qReg)]) reqQReg

	validateGateAppOverQReg :: GateApp -> QReg -> Maybe [QubitName]
	validateGateAppOverQReg (GateApplicationOverNamedQubit _ t cs) qReg =
		let errors = checkSymbolsInQReg (t:cs) [] 
		in if null errors then Nothing else Just errors
		where
			checkSymbolsInQReg :: [QubitName] -> QReg -> QReg -- Returns a list if qubit names not found in the register
			checkSymbolsInQReg [] currErrors = currErrors
			checkSymbolsInQReg (q:qs) currErrors = checkSymbolsInQReg qs (([q | q `notElem` qReg]) ++ currErrors)

	type Circ = [GateApp]
	data CircOverQReg = CircOverQReg {
		circ :: Circ,
		qReg :: QReg
	}

	instance Show CircOverQReg where
		show (CircOverQReg circ qReg) = "QCirc with qubit names: " ++ show qReg ++ "\n\t" ++ intercalate "\n\t" (map show circ)

	validateCircOverQReg :: CircOverQReg -> Maybe [QubitName]
	validateCircOverQReg (CircOverQReg gates qReg) = concat <$> mapM (`validateGateAppOverQReg` qReg) gates

	applyCircToCirc :: Circ -> Circ -> Circ
	applyCircToCirc c = (c ++)

	infixl 7 .~
	(.~) :: Circ -> QReg -> CircOverQReg
	(.~) = CircOverQReg

	(.$) :: Gate -> Qu -> [Qu] -> Circ
	(.$) g q cs = [GateApplicationOverNamedQubit g q cs]

	(.!) :: Circ -> Circ
	(.!) = inv

	(.<) :: Circ -> Circ
	(.<) = rev

	-- Define some gates
	type SingleQuGate = (Qu -> Circ)
	type DoubleQuGate = (Qu -> Qu -> Circ)
	type TripleQuGate = (Qu -> Qu -> Qu -> Circ)
	type NQuGate = (QReg -> Circ)

	h :: SingleQuGate
	h q = (.$) H q []

	x :: SingleQuGate
	x q = (.$) X q []

	unconX :: SingleQuGate
	unconX q = (.$) UnconX q []

	y :: SingleQuGate
	y q = (.$) Y q []

	z :: SingleQuGate
	z q = (.$) Z q []

	rm :: Int -> SingleQuGate
	rm m q = (.$) (Rm m) q []

	cnot :: DoubleQuGate
	cnot c t = (.$) X t [c]

	swap :: DoubleQuGate
	swap q0 q1 = 
		cnot q0 q1 ++
		cnot q1 q0 ++ 
		cnot q0 q1

	toff :: TripleQuGate
	toff c0 c1 t = (.$) X t [c0,c1]

	nqCNot :: NQuGate
	nqCNot qReg = (.$) X (last qReg) (init qReg)

	cnotTile :: QReg -> QReg -> Circ
	cnotTile cs ts = concat $ zipWith cnot cs ts

	controls :: QReg -> Circ -> Circ
	controls qReg = map (\(GateApplicationOverNamedQubit g t cs) -> GateApplicationOverNamedQubit g t (cs++qReg))

	ifQs :: QReg -> Circ -> Circ
	ifQs = controls

	controlTile :: SingleQuGate -> [QReg] -> QReg -> Circ
	controlTile g css ts = concat $ zipWith (\c t -> controls c (g t)) css ts

	negControls :: QReg -> Circ -> Circ
	negControls qReg circ = 
		loopSingleQubitGateOverReg unconX qReg ++ 
		controls qReg circ ++
		loopSingleQubitGateOverReg unconX qReg
		
	ifNotQs :: QReg -> Circ -> Circ
	ifNotQs = negControls
	
	control :: Qu -> Circ -> Circ
	control q = controls [q]

	ifQ :: Qu -> Circ -> Circ
	ifQ = control

	negControl :: Qu -> Circ -> Circ
	negControl q = negControls [q]

	ifNotQ :: Qu -> Circ -> Circ
	ifNotQ = negControl

	controlQRegPattern :: QReg -> String -> Circ -> Circ
	controlQRegPattern qReg bitPattern = let
		oneControls = concat $ zipWith (\q b -> [q | b == '1']) qReg bitPattern
		zeroControls = concat $ zipWith (\q b -> [q | b == '0']) qReg bitPattern
		in negControls zeroControls . controls oneControls

	infixl 7 .==
	(.==) :: QReg -> String -> Circ -> Circ
	(.==) = controlQRegPattern

	repeatCirc :: Int -> Circ -> Circ
	repeatCirc n c = concat $ replicate n c

	loop :: NQuGate -> [QReg] -> Circ
	loop = concatMap

	loopSingleQubitGateOverReg :: SingleQuGate -> QReg -> Circ
	loopSingleQubitGateOverReg = concatMap

	ladderQC :: Int -> Int -> NQuGate -> QReg -> Circ
	ladderQC ladderRank inputSize f qReg
		| length qReg < inputSize = []
		| length qReg == inputSize = f qReg
		| otherwise = f (take inputSize qReg) ++ ladderQC ladderRank inputSize f (drop ladderRank qReg)

	reverseLadderQC :: Int -> Int -> NQuGate -> QReg -> Circ
	reverseLadderQC ladderRank inputSize f qReg
		| length qReg == inputSize = f qReg
		| otherwise = f (takeLast inputSize qReg) ++ reverseLadderQC ladderRank inputSize f (dropLast ladderRank qReg)

	rev :: Circ -> Circ
	rev = reverse

	gateInv :: GateApp -> GateApp
	gateInv (GateApplicationOverNamedQubit g t cs) = GateApplicationOverNamedQubit (inverse g) t cs

	inv :: Circ -> Circ
	inv = rev . map gateInv

	reduceToIndexedCircuit :: CircOverQReg -> Circuit
	reduceToIndexedCircuit (CircOverQReg circ qReg) = Circuit (length qReg) $ mapNamedGateApplications qReg circ

	quRange :: QReg -> Qu -> Qu -> QReg
	quRange qReg initialQubit finalQubit = let 
		q0Index = case elemIndex initialQubit qReg of
			Just i -> i
			Nothing -> error $ "qubitRange: initialQubit name \"" ++ initialQubit ++ "\" not found in supplied qubit register " ++ show qReg
		q1Index = case elemIndex finalQubit qReg of
			Just i -> i
			Nothing -> error $ "qubitRange: finalQubit name \"" ++ finalQubit ++ "\" not found in supplied qubit register " ++ show qReg
		in if q0Index > q1Index then error $ "qubitRange: initialQubit name \"" ++ initialQubit ++ "\" is after finalQubit name \"" ++ finalQubit ++ "\" in supplied qubit register " ++ show qReg else drop q0Index . take (q1Index+1) $ qReg

	readFirstOneResult :: Int -> State -> String
	readFirstOneResult = readOneResultFromState

	readFirstOneResultOverRegister :: QReg -> QReg -> State -> String
	readFirstOneResultOverRegister = readOneResultFromStateOverRegister

	readSubregister :: QReg -> QReg -> String -> String
	readSubregister qReg reqQReg state = concatMap (\q -> [state !! fromJust (q `elemIndex` qReg)]) reqQReg

	mapInputQubitsToInputStatesOverRegister :: QReg -> [(QReg, String)] -> Int
	mapInputQubitsToInputStatesOverRegister qReg preps = let
		(combinedPreparedQReg, combinedPreparedValue) = (concatMap fst preps, concatMap snd preps)
		fullPreparedQReg = combinedPreparedQReg ++ (qReg \\ combinedPreparedQReg)
		fullPreparedValue = combinedPreparedValue ++ (replicate (length qReg - length combinedPreparedQReg) '0')
		reorderedState = map (\q -> fullPreparedValue !! fromJust (q `elemIndex` fullPreparedQReg)) qReg
		in bitStringToInt reorderedState

	readSubregisterResultFromInt :: QReg -> QReg -> Int -> Int
	readSubregisterResultFromInt qReg reqQReg state = let
		stateString = paddedIntToBitString (length qReg) state
		requiredState = readSubregister qReg reqQReg stateString
		in bitStringToInt requiredState

	makeQRegOverID :: Int -> String -> QReg
	makeQRegOverID n identifier = let r = map (\x -> identifier ++ show x) [0..n-1] in r
