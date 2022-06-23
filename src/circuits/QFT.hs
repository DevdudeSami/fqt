module QFT (qft) where

	import QCEDSL

	qft :: NQuGate
	qft qReg = let n = length qReg in 
		concat $ concat [h (qReg!!i) : [control (qReg!!j) (rm (j-i+1) (qReg!!i)) | j <- [i+1..n-1]] | i <- [0..n-1]]
