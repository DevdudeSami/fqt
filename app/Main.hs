{-|
    Module      : Main
    Description : Defines the entry point of the app.
-}
module Main where
	import QCEDSL
	import QCHelpers
	import QFT
	import QWMEngine

	import QCMath
	import LogicQCSimulator

	import QCTesting

	import Helpers

	main :: IO ()
	main = do
		print "Run `stack test` to run the circuit unit tests. Or define your circuit in the main file."

		print "Running 5-Qubit QFT"
		let
			qReg = makeQRegOverID 5 "q"
			circ = qft qReg
			indexedCirc = reduceToIndexedCircuit (circ .~ qReg)
			outputState = preprocessAndRunCircuitOnZeroState indexedCirc
		print circ
		print outputState

		print "Running 32-Bit Full Adder with Logic-only simulator"
		let 
			aReg = makeQRegOverID 32 "a"
			bReg = makeQRegOverID 32 "b"
			c = "c"
			z = "z"
			fullReg = aReg ++ bReg ++ [c, z]
			adderCirc = fullAdd aReg bReg c z
			outputBitRegister = prepAndRunLogicQC [
					(reverse aReg, paddedIntToBitString 32 20), 
					(reverse bReg, paddedIntToBitString 32 22)
				] adderCirc fullReg
			outputB = readSubregister fullReg (reverse bReg) outputBitRegister
		print adderCirc
		print $ bitRegisterToString outputB
		print $ bitStringToInt $ bitRegisterToString outputB