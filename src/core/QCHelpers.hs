module QCHelpers (
	Qu3, Qu4, Qu5, Qu8,
	toList3, toList4, toList5, toList8,
	make3QubitFloatingPointReg
) where

	import QCEDSL

	type Qu3 = (Qu, Qu, Qu)
	type Qu4 = (Qu, Qu, Qu, Qu)
	type Qu5 = (Qu, Qu, Qu, Qu, Qu)
	type Qu8 = (Qu, Qu, Qu, Qu, Qu, Qu, Qu, Qu)

	toList3 :: (a,a,a) -> [a]
	toList3 (a,b,c) = [a,b,c]
	toList4 :: (a,a,a,a) -> [a]
	toList4 (a,b,c,d) = [a,b,c,d]
	toList5 :: (a,a,a,a,a) -> [a]
	toList5 (a,b,c,d,e) = [a,b,c,d,e]
	toList8 :: (a,a,a,a,a,a,a,a) -> [a]
	toList8 (a,b,c,d,e,f,g,h) = [a,b,c,d,e,f,g,h]

	make3QubitFloatingPointReg :: String -> (Qu3, Qu, Qu3)
	make3QubitFloatingPointReg name = let
		[e0,e1,e2] = makeQRegOverID 3 ('e':name)
		[m0,m1,m2] = makeQRegOverID 3 ('m':name)
		sign = 's':name
		in ((e0,e1,e2), sign, (m0,m1,m2))
