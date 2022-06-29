module QCTesting (
	QCTest(..), QCTestSuite(..),
	Prep, Expec,
	runQC, prepAndRunQC,
	runLogicQC, prepAndRunLogicQC,
	testQC,
	runQCTest, runLogicQCTest, runQCTestSuite, runQCTestSuites
	) where

	import QCEDSL
	import Definitions
	import QWMEngine
	import LogicQCSimulator
	import Helpers

	import Data.List (intercalate)

	type Prep = (QReg, String)
	type Expec = (QReg, String)

	showPrepWithInts :: (QReg, String) -> String
	showPrepWithInts (q, s) = "(|" ++ (intercalate "|" q) ++ ">: " ++ s ++ " (" ++ show (bitStringToInt s) ++ "))"

	data QCTest = QCTest {
		name :: String,
		preps :: [Prep],
		expecs :: [Expec]
	}

	data QCTestSuite = QCTestSuite {
		description :: String,
		circ :: Circ,
		qRegister :: QReg,
		tests :: [QCTest]
	}

	runQC :: Circ -> QReg -> State
	runQC circ qr = let c = circ .~ qr in
		preprocessAndRunCircuitOnZeroState $ reduceToIndexedCircuit c

	runLogicQC :: Circ -> QReg -> BitRegister
	runLogicQC circ qr = let c = circ .~ qr in
		simulateLogicQC $ preprocessCircuit $ reduceToIndexedCircuit c

	prepAndRunQC :: [Prep] -> Circ -> QReg -> State
	prepAndRunQC preps c qr = let
		prepGates = concat $ concatMap (uncurry $ zipWith (\q val -> if val == '1' then x q else [])) preps
		in runQC (prepGates ++ c) qr

	prepAndRunLogicQC :: [Prep] -> Circ -> QReg -> BitRegister
	prepAndRunLogicQC preps c qr = let
		prepGates = concat $ concatMap (uncurry $ zipWith (\q val -> if val == '1' then x q else [])) preps
		in runLogicQC (prepGates ++ c) qr

	testQC :: QCTest -> Circ -> QReg -> Bool
	testQC (QCTest _ preps expecs) c qr = let
		fullResult = prepAndRunQC preps c qr
		in all (\(qReg, expectedResult) -> readFirstOneResultOverRegister qr qReg fullResult == expectedResult) expecs

	testLogicQC :: QCTest -> Circ -> QReg -> Bool
	testLogicQC (QCTest _ preps expecs) c qr = let
		resultState = prepAndRunLogicQC preps c qr
		in all (\(qReg, expectedResult) -> bitRegisterToString (readSubregister qr qReg resultState) == expectedResult) expecs

	runQCTest :: QCTest -> Circ -> QReg -> IO (Bool)
	runQCTest (QCTest name preps expecs) c qr = let 
		fullResult = prepAndRunQC preps c qr
		expectationResults = map (\(qReg, _) -> (qReg, readFirstOneResultOverRegister qr qReg fullResult)) expecs
		passed = all (\(qReg, expectedResult) -> readFirstOneResultOverRegister qr qReg fullResult == expectedResult) expecs
		in do
		putStrLn $ "Running test: " ++ name
		putStrLn $ "Preparations: " ++ show (map showPrepWithInts preps)
		putStrLn $ "Expectations: " ++ show (map showPrepWithInts expecs)
		putStrLn $ "Expectation results: " ++ show (map showPrepWithInts expectationResults)
		putStrLn $ "Passed: " ++ show passed
		putStrLn ""
		return passed
	
	runLogicQCTest :: QCTest -> Circ -> QReg -> IO (Bool)
	runLogicQCTest (QCTest name preps expecs) c qr = let 
		resultState = prepAndRunLogicQC preps c qr
		expectationResults = map (\(qReg, _) -> (qReg, bitRegisterToString (readSubregister qr qReg resultState))) expecs
		passed = all (\(qReg, expectedResult) -> bitRegisterToString (readSubregister qr qReg resultState) == expectedResult) expecs
		in do
		putStrLn $ "Running test using LogicQCSimulator: " ++ name
		putStrLn $ "Preparations: " ++ show (map showPrepWithInts preps)
		putStrLn $ "Expectations: " ++ show (map showPrepWithInts expecs)
		putStrLn $ "Expectation results: " ++ show (map showPrepWithInts expectationResults)
		putStrLn $ "Result state: |" ++ intercalate "|" (zipWith (\q s -> q ++ ":" ++ s) qr (map pure (bitRegisterToString resultState))) ++ ">"
		putStrLn $ "Passed: " ++ show passed
		putStrLn ""
		return passed

	runQCTestSuite :: Bool -> QCTestSuite -> IO ()
	runQCTestSuite isLogicQC (QCTestSuite description circ qReg tests) = 
		let runFunc = if isLogicQC then runLogicQCTest else runQCTest in do
		putStrLn $ "Running test suite: " ++ description
		print (circ .~ qReg)
		putStrLn $ "Preprocessed circuit: " ++ show (preprocessCircuit $ reduceToIndexedCircuit (circ .~ qReg))
		putStrLn ""
		passes <- mapM (\t -> runFunc t circ qReg) tests
		let passingTests = length $ filter id passes
		putStrLn $ "Tests passed in suite: " ++ show passingTests ++ " out of " ++ show (length tests)
		putStrLn ""
	
	runQCTestSuites :: Bool -> [QCTestSuite] -> IO ()
	runQCTestSuites = mapM_ . runQCTestSuite 
