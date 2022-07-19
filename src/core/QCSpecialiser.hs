module QCSpecialiser (
	specialiseQCircForQubitValue, specialiseQCircForQubitValues
) where

	import QCEDSL
	import Definitions

	-- The target specialisation qubit can be used as the control for any other qubit but must not have a gate applied to it other than NOT with no controls.
	specialiseQCircForQubitValue :: Qu -> Char -> Circ -> Circ
	specialiseQCircForQubitValue q v c = let
		go :: Char -> Circ -> Circ -> Circ
		go _ [] acc = acc
		go valCheck ((GateApplicationOverNamedQubit gate t cs):gs) acc = 
			if t == q && gate == UnconX 
			then go (if valCheck == '0' then '1' else '0') gs acc 
			else let
				qIsAControl = q `elem` cs
				includeGate = (not qIsAControl || v == valCheck) && q /= t
				in if includeGate
					then go valCheck gs (acc ++ [GateApplicationOverNamedQubit gate t (filter (/= q) cs)])
					else go valCheck gs acc
		in go '1' c []

	specialiseQCircForQubitValues :: QReg -> String -> Circ -> Circ
	specialiseQCircForQubitValues [] [] c = c
	specialiseQCircForQubitValues (q:qs) (v:vs) c = specialiseQCircForQubitValue q v (specialiseQCircForQubitValues qs vs c)