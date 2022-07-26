module QFTAdder (
	qftFullAdd
) where

	import QCEDSL
	import QFT
	import Helpers

	qftFullAdd :: QReg -> QReg -> Qu -> Circ
	qftFullAdd in1 in2 z = let 
		adderControlledPhases = concat [
			control (in1!!i) (rm (j+1) (in2!!(i+j))) 
			| i <- [0..length in1-1], 
				j <- [0..length in1-1 - i]
			]
		carryBitPhases = concat [
			control (in1!!(length in1-1 - i)) (rm (i+2) z)
			| i <- [0..length in1-1]
			]
		in
		qft (reverse (in2 ++ [z])) ++
		adderControlledPhases ++
		carryBitPhases ++
		(.!) (qft (reverse (in2 ++ [z])))

