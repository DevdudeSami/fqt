module QCSplitter (
	splitQCircOverQubit
) where

	import QCEDSL

	splitQCircOverQubit :: Qu -> Char -> Circ -> Circ
	splitQCircOverQubit q v c = let
		go :: Circ -> Circ -> Circ
		go [] acc = acc
		go ((GateApplicationOverNamedQubit gate t cs):gs) acc = let
			qIsAControl = q `elem` cs
			includeGate = ((not qIsAControl) || v == '1') && q /= t
			in if includeGate
				then go gs (acc ++ [GateApplicationOverNamedQubit gate t (filter (/= q) cs)])
				else go gs acc
		in go c []