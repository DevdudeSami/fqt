module QCGrovers (groversSearch, groversSearchForNumber) where

	import QCEDSL
	import Helpers

	-- | Defines Grover's Diffusion operator for Grover's Search algorithm. Takes the search register only **without the ancilla qubit**.
	groversDiffusion :: QReg -> Circ
	groversDiffusion qReg = 
		loopSingleQubitGateOverReg h qReg ++
		loopSingleQubitGateOverReg x qReg ++
		controls (dropLast 1 qReg) (z (last qReg)) ++
		loopSingleQubitGateOverReg x qReg ++
		loopSingleQubitGateOverReg h qReg

 	groversOracle :: QReg -> Qu -> QReg -> Circ
	groversOracle qReg anc onQubits =
		loopSingleQubitGateOverReg x onQubits ++
		controls qReg (x anc) ++
		loopSingleQubitGateOverReg x onQubits

	groversSearch :: QReg -> Qu -> QReg -> Int -> Circ
	groversSearch searchReg anc onQubits iterations =
		x anc ++ -- ancilla qubit needs to be in the |1> state
		loopSingleQubitGateOverReg h searchReg ++ h anc ++
		repeatCirc iterations (
			groversOracle searchReg anc onQubits ++
			groversDiffusion searchReg
		)

	groversSearchForNumber :: QReg -> Qu -> Int -> Int -> Circ
	groversSearchForNumber searchReg anc searchTarget iterations = let
		searchBitString = paddedIntToBitString (length searchReg) searchTarget
		onQubits = map snd $ filter (\(i, _) -> searchBitString!!i == '1') (zip [0..] searchReg)
		in groversSearch searchReg anc onQubits iterations