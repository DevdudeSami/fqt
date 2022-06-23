module LogicQCSimulator (
	Bit(..), BitRegister,
	toggle, toggleAt,
	isOne, isZero,
	bitRegisterToString,
	simulateLogicGate, simulateLogicQC,
	simulateLogicNamedGateApplication,
	readSubregister,
	bitToChar
	) where

	import Definitions
	import QCEDSL

	import Data.Maybe ( fromJust )
	import Data.List ( elemIndex )

	data Bit = Zero | One
		deriving (Eq, Ord, Show)
	type BitRegister = [Bit]

	toggle :: Bit -> Bit
	toggle Zero = One
	toggle One = Zero

	isOne :: Bit -> Bool
	isOne One = True
	isOne _ = False

	isZero :: Bit -> Bool
	isZero Zero = True
	isZero _ = False

	bitToChar :: Bit -> Char
	bitToChar Zero = '0'
	bitToChar One = '1'

	bitToString :: Bit -> String
	bitToString Zero = "0"
	bitToString One = "1"

	bitRegisterToString :: BitRegister -> String
	bitRegisterToString = map bitToChar

	toggleAt :: Int -> BitRegister -> BitRegister
	toggleAt index bits = take index bits ++ [toggle $ bits !! index] ++ drop (index + 1) bits

	simulateLogicGate :: GateApplication -> BitRegister -> BitRegister
	simulateLogicGate (GateApplication g t cs _) input 
		| g /= X = error $ "simulateLogicGate: gate must be X, found: " ++ show g
		| all (\c -> isOne (input!!c)) cs = toggleAt t input
		| otherwise = input

	simulateLogicNamedGateApplication :: GateApp -> QReg -> BitRegister -> BitRegister
	simulateLogicNamedGateApplication (GateApplicationOverNamedQubit g t cs) qReg input 
		| g /= X = error $ "simulateLogicNamedGateApplication: gate must be X, found: " ++ show g
		| all (\c -> isOne (input!!fromJust (c `elemIndex` qReg))) cs = toggleAt (fromJust $ t `elemIndex` qReg) input
		| otherwise = input

	simulateLogicQC :: Circuit -> BitRegister
	simulateLogicQC (Circuit n gs) = simulateLogicQC' gs (replicate n Zero)
		where 
			simulateLogicQC' :: [GateApplication] -> BitRegister -> BitRegister
			simulateLogicQC' [] input = input
			simulateLogicQC' (ga:gas) input = simulateLogicQC' gas (simulateLogicGate ga input)

	readSubregister :: QReg -> QReg -> BitRegister -> BitRegister
	readSubregister qReg reqQReg state = concatMap (\q -> [state !! fromJust (q `elemIndex` qReg)]) reqQReg