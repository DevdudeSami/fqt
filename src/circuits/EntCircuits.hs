module EntCircuits (entangle, entangleWithBase) where

	import QCEDSL

	entangle :: NQuGate
	entangle qReg = 
		h (head qReg) ++
		control (head qReg) (loopSingleQubitGateOverReg x (tail qReg))

	entangleWithBase :: Qu -> NQuGate
	entangleWithBase base otherQubits = entangle (base:otherQubits)