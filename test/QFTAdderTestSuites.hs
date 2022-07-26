module QFTAdderTestSuites (qftFullAdderTestSuite, specialisedQFTFullAdderTestSuites) where

	import QCEDSL
	import QCTesting
	import QFTAdder

	import QCSpecialiser
        import QCSpecialiser (specialiseQCircForQubitValues)
        import Helpers (paddedIntToBitString)
        
	qftFullAdderTestSuite :: QCTestSuite
	qftFullAdderTestSuite = let
		aReg = makeQRegOverID 4 "a"
		bReg = makeQRegOverID 4 "b"
		z = "z"
		fullRegister = aReg ++ bReg ++ [z]
		in QCTestSuite "4-Bit Full QFT Draper Adder Test Suite" (qftFullAdd aReg bReg z) fullRegister [
			QCTest "4-Bit QFT Draper Test: 1 + 8 = 9" 
			[
				(reverse aReg, "0001"),
				(reverse bReg, "1000")
			] 
			[(reverse bReg, "1001")],
			QCTest "4-Bit QFT Draper Test: 2 + 2 = 4" 
			[
				(reverse aReg, "0010"),
				(reverse bReg, "0010")
			]
			[(reverse bReg, "0100")],
			QCTest "4-Bit QFT Draper Test: 8 + 8 = 16"
			[
				(reverse aReg, "1000"),
				(reverse bReg, "1000")
			]
			[(reverse bReg, "0000"), ([z], "1")]
		]

	specialisedQFTFullAdderTestSuites :: [QCTestSuite]
	specialisedQFTFullAdderTestSuites = let 
		aReg = makeQRegOverID 4 "a"
		bReg = makeQRegOverID 4 "b"
		z = "z"
		fullRegister = aReg ++ bReg ++ [z]
		fullCirc = qftFullAdd aReg bReg z

		-- Specialise on a
		specCircs = [specialiseQCircForQubitValues (reverse aReg) (paddedIntToBitString 4 i) fullCirc | i <- [0..15]]
		specRegister = bReg ++ [z]

		in [
		 	QCTestSuite "AutoSpecialised 4-Bit Full QFT Draper Adder Test Suite" (specCircs!!3) specRegister [
				QCTest "4-Bit QFT Draper Test: 3 + 8 = 11" 
				[
					(reverse bReg, "1000")
				] 
				[(reverse bReg, "1011")],
				QCTest "4-Bit QFT Draper Test: 3 + 2 = 5" 
				[
					(reverse bReg, "0010")
				]
				[(reverse bReg, "0101")],
				QCTest "4-Bit QFT Draper Test: 3 + 15 = 18"
				[
					(reverse bReg, "1111")
				]
				[(reverse bReg, "0010"), ([z], "1")]
			],
			QCTestSuite "AutoSpecialised 4-Bit Full QFT Draper Adder Test Suite" (specCircs!!15) specRegister [
				QCTest "4-Bit QFT Draper Test: 15 + 8 = 23" 
				[
					(reverse bReg, "1000")
				] 
				[(reverse bReg, "0111"), ([z], "1")],
				QCTest "4-Bit QFT Draper Test: 15 + 2 = 17" 
				[
					(reverse bReg, "0010")
				]
				[(reverse bReg, "0001"), ([z], "1")],
				QCTest "4-Bit QFT Draper Test: 15 + 15 = 30"
				[
					(reverse bReg, "1111")
				]
				[(reverse bReg, "1110"), ([z], "1")]
			]

		]