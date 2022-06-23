module QCTestSuites (
	mathTestSuites,
	registerShiftTestSuites
	) where

	import QCEDSL
	import QCTesting
	import QCMath
	import Helpers

	mathTestSuites :: [QCTestSuite]
	mathTestSuites = [square4TestSuite, squareAndShift4TestSuite, modAdder4TestSuite, modAdder5TestSuite]

	registerShiftTestSuites :: [QCTestSuite]
	registerShiftTestSuites = [leftShiftTestSuite, oddLeftShiftTestSuite, rightShiftTestSuite, invLeftShiftTestSuite]

	fullAdderTestSuite :: QCTestSuite
	fullAdderTestSuite = let
		aReg = makeQRegOverID 4 "a"
		bReg = makeQRegOverID 4 "b"
		c = "c"
		z = "z"
		fullRegister = aReg ++ bReg ++ [c] ++ [z]
		in QCTestSuite "4-Bit Full Cuccaro Adder Test Suite" (fullAdd aReg bReg c z) fullRegister [
			QCTest "4-Bit Cuccaro Test: 1 + 8 = 9" 
			[
				(reverse aReg, "0001"),
				(reverse bReg, "1000")
			] 
			[(reverse bReg, "1001")],
			QCTest "4-Bit Cuccaro Test: 2 + 2 = 4" 
			[
				(reverse aReg, "0010"),
				(reverse bReg, "0010")
			]
			[(reverse bReg, "0100")],
			QCTest "4-Bit Cuccaro Test: 8 + 8 = 16"
			[
				(reverse aReg, "1000"),
				(reverse bReg, "1000")
			]
			[(reverse bReg, "0000"), ([z], "1")]
		]

	modAdder4TestSuite :: QCTestSuite
	modAdder4TestSuite = let
		aReg = makeQRegOverID 4 "a"
		bReg = makeQRegOverID 4 "b"
		c = "c"
		fullRegister = aReg ++ bReg ++ [c]
		in QCTestSuite "4-Bit Modulo Cuccaro Adder Test Suite" (modAdd aReg bReg c) fullRegister [
			QCTest "4-Bit Modulo Cuccaro Adder Test: (1 + 8) mod 16 = 9" 
			[
				(reverse aReg, "0001"),
				(reverse bReg, "1000")
			] 
			[(reverse bReg, "1001"), (reverse aReg, "0001"), ([c], "0")],
			QCTest "4-Bit Modulo Cuccaro Test: (2 + 2) mod 16 = 4" 
			[
				(reverse aReg, "0010"),
				(reverse bReg, "0010")
			]
			[(reverse bReg, "0100"), (reverse aReg, "0010"), ([c], "0")],
			QCTest "4-Bit Modulo Cuccaro Test: (8 + 8) mod 16 = 0"
			[
				(reverse aReg, "1000"),
				(reverse bReg, "1000")
			]
			[(reverse bReg, "0000"), (reverse aReg, "1000"), ([c], "0")],
			QCTest "4-Bit Modulo Cuccaro Test: (15 + 15) mod 16 = 14"
			[
				(reverse aReg, "1111"),
				(reverse bReg, "1111")
			]
			[(reverse bReg, "1110"), (reverse aReg, "1111"), ([c], "0")]
		]

	modAdder5TestSuite :: QCTestSuite
	modAdder5TestSuite = let
		aReg = makeQRegOverID 5 "a"
		bReg = makeQRegOverID 5 "b"
		c = "c"
		fullRegister = aReg ++ bReg ++ [c]
		in QCTestSuite "5-Bit Modulo Cuccaro Adder Test Suite" (modAdd aReg bReg c) fullRegister [
			QCTest "5-Bit Modulo Cuccaro Adder Test: (1 + 16) mod 32 = 17" 
			[
				(reverse aReg, "00001"),
				(reverse bReg, "10000")
			] 
			[(reverse bReg, "10001"), (reverse aReg, "00001"), ([c], "0")],
			QCTest "5-Bit Modulo Cuccaro Test: (2 + 2) mod 32 = 4" 
			[
				(reverse aReg, "00010"),
				(reverse bReg, "00010")
			]
			[(reverse bReg, "00100"), (reverse aReg, "00010"), ([c], "0")],
			QCTest "4-Bit Modulo Cuccaro Test: (16 + 16) mod 32 = 0"
			[
				(reverse aReg, "10000"),
				(reverse bReg, "10000")
			]
			[(reverse bReg, "00000"), (reverse aReg, "10000"), ([c], "0")],
			QCTest "4-Bit Modulo Cuccaro Test: (31 + 31) mod 32 = 30"
			[
				(reverse aReg, "11111"),
				(reverse bReg, "11111")
			]
			[(reverse bReg, "11110"), (reverse aReg, "11111"), ([c], "0")]
		]

	leftShiftTestSuite :: QCTestSuite
	leftShiftTestSuite = let
		reg = makeQRegOverID 4 "q"
		in QCTestSuite "4-Bit Left-shift Test Suite" (leftShift reg) reg [
			QCTest "4-Bit Left-shift test: 0001 -> 0010"
				[(reg, "0001")] 
				[(reg, "0010")],
			QCTest "4-Bit Left-shift test: 0010 -> 0100"
				[(reg, "0010")]
				[(reg, "0100")], 
			QCTest "4-Bit Left-shift test: 0100 -> 1000"
				[(reg, "0100")] 
				[(reg, "1000")],
			QCTest "4-Bit Left-shift test: 1000 -> 0001"
				[(reg, "1000")] 
				[(reg, "0001")]
			]

	invLeftShiftTestSuite :: QCTestSuite
	invLeftShiftTestSuite = let
		reg = makeQRegOverID 4 "q"
		in QCTestSuite "4-Bit Inverse Left-shift Test Suite" ((.!) $ leftShift reg) reg [
			QCTest "4-Bit Inverse Left-shift test: 0010 -> 0001"
				[(reg, "0010")]
				[(reg, "0001")], 
			QCTest "4-Bit Inverse Left-shift test: 0100 -> 0010"
				[(reg, "0100")] 
				[(reg, "0010")],
			QCTest "4-Bit Inverse Left-shift test: 1000 -> 0100"
				[(reg, "1000")]
				[(reg, "0100")], 
			QCTest "4-Bit Inverse Left-shift test: 0001 -> 1000"
				[(reg, "0001")]
				[(reg, "1000")] 
			]

	oddLeftShiftTestSuite :: QCTestSuite
	oddLeftShiftTestSuite = let
		reg = makeQRegOverID 5 "q"
		in QCTestSuite "5-Bit Left-shift Test Suite" (leftShift reg) reg [
			QCTest "5-Bit Left-shift test: 00001 -> 00010"
				[(reg, "00001")] 
				[(reg, "00010")],
			QCTest "5-Bit Left-shift test: 00010 -> 00100"
				[(reg, "00010")]
				[(reg, "00100")], 
			QCTest "5-Bit Left-shift test: 00100 -> 01000"
				[(reg, "00100")] 
				[(reg, "01000")],
			QCTest "5-Bit Left-shift test: 01000 -> 10000"
				[(reg, "01000")] 
				[(reg, "10000")],
			QCTest "5-Bit Left-shift test: 10000 -> 00001"
				[(reg, "10000")] 
				[(reg, "00001")]
		]

	rightShiftTestSuite :: QCTestSuite
	rightShiftTestSuite = let
		reg = makeQRegOverID 4 "q"
		in QCTestSuite "4-Bit Right-shift Test Suite" (rightShift reg) reg [
			QCTest "4-Bit Right-shift test: 0010 -> 0001"
				[(reg, "0010")]
				[(reg, "0001")], 
			QCTest "4-Bit Right-shift test: 0100 -> 0010"
				[(reg, "0100")] 
				[(reg, "0010")],
			QCTest "4-Bit Right-shift test: 1000 -> 0100"
				[(reg, "1000")]
				[(reg, "0100")], 
			QCTest "4-Bit Right-shift test: 0001 -> 1000"
				[(reg, "0001")]
				[(reg, "1000")] 
		]

	square2TestSuite :: QCTestSuite
	square2TestSuite = let
		uReg = makeQRegOverID 2 "u"
		rReg = makeQRegOverID 4 "r"
		[r0, r1, r2, r3] = rReg
		resultRegArrangement = [r3, r0, r1, r2]
		c = "c"
		anc = "anc"
		in QCTestSuite "2-Bit Square Circuit Test Suite" (square uReg rReg c anc) (uReg ++ rReg ++ [c] ++ [anc]) [
			QCTest "2-Bit Square Test: 0^2 = 0"
				[(reverse uReg, "00")]
				[(reverse resultRegArrangement, "0000")], 
			QCTest "2-Bit Square Test: 1^2 = 1"
				[(reverse uReg, "01")] 
				[(reverse resultRegArrangement, "0001")], 
			QCTest "2-Bit Square Test: 2^2 = 4"
				[(reverse uReg, "10")]
				[(reverse resultRegArrangement, "0100")],
			QCTest "2-Bit Square Test: 3^2 = 9"
				[(reverse uReg, "11")]
				[(reverse resultRegArrangement, "1001")]
		]

	squareISquare2TestSuite :: QCTestSuite
	squareISquare2TestSuite = let
		uReg = makeQRegOverID 2 "u"
		rReg = makeQRegOverID 4 "r"
		[r0, r1, r2, r3] = rReg
		resultRegArrangement = [r3, r0, r1, r2]
		c = "c"
		anc = "anc"
		in QCTestSuite "2-Bit Square followed by Inverse Square Circuit Test Suite" 
			(square uReg rReg c anc ++ inv (square uReg rReg c anc)) 
			(uReg ++ rReg ++ [c] ++ [anc]) [
			QCTest "2-Bit Square then ISquare Test: 0"
				[(reverse uReg, "00")]
				[(reverse uReg, "00"), (reverse rReg, "0000"), ([c,anc], "00")],
			QCTest "2-Bit Square then ISquare Test: 1"
				[(reverse uReg, "01")]
				[(reverse uReg, "01"), (reverse rReg, "0000"), ([c,anc], "00")],
			QCTest "2-Bit Square then ISquare Test: 2"
				[(reverse uReg, "10")]
				[(reverse uReg, "10"), (reverse rReg, "0000"), ([c,anc], "00")],
			QCTest "2-Bit Square then ISquare Test: 3"	
				[(reverse uReg, "11")]
				[(reverse uReg, "11"), (reverse rReg, "0000"), ([c,anc], "00")]
			]

	iSquare2TestSuite :: QCTestSuite
	iSquare2TestSuite = let
		uReg = makeQRegOverID 2 "u"
		rReg = makeQRegOverID 4 "r"
		[r0, r1, r2, r3] = rReg
		resultRegArrangement = [r3, r0, r1, r2]
		c = "c"
		anc = "anc"
		in QCTestSuite "2-Bit Inverse Square Circuit Test Suite" (inv $ square uReg rReg c anc) (uReg ++ rReg ++ [c] ++ [anc]) [
			QCTest "2-Bit Inverse Square Test: sqrt(0) = 0"
				[(reverse resultRegArrangement, "0000"), (reverse uReg, "00")]
				[(reverse uReg, "00"), (reverse rReg, "0000")],
			QCTest "2-Bit Inverse Square Test: sqrt(1) = 1"
				[(reverse resultRegArrangement, "0001"), (reverse uReg, "01")]
				[(reverse uReg, "01"), (reverse rReg, "0000")],
			QCTest "2-Bit Inverse Square Test: sqrt(4) = 2"
				[(reverse resultRegArrangement, "0100"), (reverse uReg, "10")]
				[(reverse uReg, "10"), (reverse rReg, "0000")],
			QCTest "2-Bit Inverse Square Test: sqrt(9) = 3"	
				[(reverse resultRegArrangement, "1001"), (reverse uReg, "11")]
				[(reverse uReg, "11"), (reverse rReg, "0000")]
			]

	square3TestSuite :: QCTestSuite
	square3TestSuite = let
		uReg = makeQRegOverID 3 "u"
		rReg = makeQRegOverID 6 "r"
		[r0, r1, r2, r3, r4, r5] = rReg
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

	squareAndShift4TestSuite :: QCTestSuite
	squareAndShift4TestSuite = let
		uReg = makeQRegOverID 4 "u"
		rReg = makeQRegOverID 8 "r"
		c = "c"
		anc = "anc"
		in QCTestSuite "4-Bit SquareAndShiftBack Circuit Test Suite" (squareAndShiftBack uReg rReg c anc) (uReg ++ rReg ++ [c] ++ [anc]) [
			QCTest ("4-Bit Square Test: " ++ (show u) ++ "^2 = " ++ (show (u^2)))
			[(reverse uReg, paddedIntToBitString 4 u)]
			[(reverse rReg, paddedIntToBitString 8 (u^2))]
			| u <- [0..15]
			]

	square5TestSuite :: QCTestSuite
	square5TestSuite = let
		uReg = makeQRegOverID 5 "u"
		rReg = makeQRegOverID 10 "r"
		resultRegArrangement = rotate (5+1) rReg
		c = "c"
		anc = "anc"
		in QCTestSuite "5-Bit Square Circuit Test Suite" (square uReg rReg c anc) (uReg ++ rReg ++ [c] ++ [anc]) [
			QCTest ("5-Bit Square Test: " ++ (show u) ++ "^2 = " ++ (show (u^2)))
			[(reverse uReg, paddedIntToBitString 5 u)]
			[(reverse resultRegArrangement, paddedIntToBitString 10 (u^2))]
			| u <- [0..31]
			]

	sgn3TestSuite :: QCTestSuite
	sgn3TestSuite = let
		reg = makeQRegOverID 3 "q"
		in QCTestSuite "3-Bit Sgn Circuit Test Suite" (sgn reg) reg [
			QCTest "3-Bit Sgn Test: 000"
				[(reverse reg, "000")]
				[(reverse reg, "000")],
			QCTest "3-Bit Sgn Test: 001"
				[(reverse reg, "001")]
				[(reverse reg, "111")],
			QCTest "3-Bit Sgn Test: 010"
				[(reverse reg, "010")]
				[(reverse reg, "110")],
			QCTest "3-Bit Sgn Test: 011"
				[(reverse reg, "011")]
				[(reverse reg, "101")],
			QCTest "3-Bit Sgn Test: 100"
				[(reverse reg, "100")]
				[(reverse reg, "100")],
			QCTest "3-Bit Sgn Test: 101"
				[(reverse reg, "101")]
				[(reverse reg, "011")],
			QCTest "3-Bit Sgn Test: 110"
				[(reverse reg, "110")]
				[(reverse reg, "010")],
			QCTest "3-Bit Sgn Test: 111"
				[(reverse reg, "111")]
				[(reverse reg, "001")]
		]

	sgn5TestSuite :: QCTestSuite
	sgn5TestSuite = let
		reg = makeQRegOverID 5 "q"
		in QCTestSuite "5-Bit Sgn Circuit Test Suite" (sgn reg) reg [
			QCTest "5-Bit Sgn Test: 00000"
				[(reverse reg, "00000")]
				[(reverse reg, "00000")],
			QCTest "5-Bit Sgn Test: 00001"
				[(reverse reg, "00001")]
				[(reverse reg, "11111")],
			QCTest "5-Bit Sgn Test: 00010"
				[(reverse reg, "00010")]
				[(reverse reg, "11110")]
		]