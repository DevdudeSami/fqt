{-|
	Module      : LogicQCSimulator
	Description : Defines a simulator that can only execute circuits with the X gate and any number of controls, i.e. a reversible or TOFFOLI circuit.
-}
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

	-- | A bit is either Zero or One.
	data Bit = Zero | One
		deriving (Eq, Ord, Show)

	-- | A bit register is a list of bits.
	type BitRegister = [Bit]

	-- | Toggle a bit.
	toggle :: Bit -> Bit
	toggle Zero = One
	toggle One = Zero

	-- | Check if a bit is one.
	isOne :: Bit -> Bool
	isOne One = True
	isOne _ = False

	-- | Check if a bit is zero.
	isZero :: Bit -> Bool
	isZero Zero = True
	isZero _ = False

	-- | Converts a Bit to a Char.
	bitToChar :: Bit -> Char
	bitToChar Zero = '0'
	bitToChar One = '1'

	-- | Converts a Bit to a String.
	bitToString :: Bit -> String
	bitToString Zero = "0"
	bitToString One = "1"

	-- | Converts a BitRegister to a String.
	bitRegisterToString :: BitRegister -> String
	bitRegisterToString = map bitToChar

	-- | Toggle a bit at a given index.
	toggleAt :: Int -> BitRegister -> BitRegister
	toggleAt index bits = take index bits ++ [toggle $ bits !! index] ++ drop (index + 1) bits

	-- | Simulate a "GateApplication" on an input "BitRegister" using the logic-only simulator.
	simulateLogicGate :: GateApplication -> BitRegister -> BitRegister
	simulateLogicGate (GateApplication g t cs _) input 
		| g /= X = error $ "simulateLogicGate: gate must be X, found: " ++ show g
		| all (\c -> isOne (input!!c)) cs = toggleAt t input
		| otherwise = input

	-- | Simulate a "GateApplicationOverNamedQubit" on "QReg" with an input "BitRegister" using the logic-only simulator.
	simulateLogicNamedGateApplication :: GateApp -> QReg -> BitRegister -> BitRegister
	simulateLogicNamedGateApplication (GateApplicationOverNamedQubit g t cs) qReg input 
		| g /= X = error $ "simulateLogicNamedGateApplication: gate must be X, found: " ++ show g
		| all (\c -> isOne (input!!fromJust (c `elemIndex` qReg))) cs = toggleAt (fromJust $ t `elemIndex` qReg) input
		| otherwise = input

	-- | Simulate a quantum "Circuit" over a zero register using the logic-only simulator.
	simulateLogicQC :: Circuit -> BitRegister
	simulateLogicQC (Circuit n gs) = simulateLogicQC' gs (replicate n Zero)
		where 
			simulateLogicQC' :: [GateApplication] -> BitRegister -> BitRegister
			simulateLogicQC' [] input = input
			simulateLogicQC' (ga:gas) input = simulateLogicQC' gas (simulateLogicGate ga input)

	-- | Read a subregister from a "BitRegister".
	readSubregister :: QReg -- ^ The named arrangement of the full BitRegister.
		-> QReg -- ^ The required subregister.
	  -> BitRegister -- ^ The register to read from.
		-> BitRegister
	readSubregister qReg reqQReg state = concatMap (\q -> [state !! fromJust (q `elemIndex` qReg)]) reqQReg