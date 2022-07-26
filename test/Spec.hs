import QCTesting
import QCTestSuites
import QFTAdderTestSuites
import Helpers

{-|
  Description : Defines the entry point of testing.
-}
main :: IO ()
main = do
	runQCTestSuite False qftFullAdderTestSuite -- Cannot use the logic-based simulator for QFT adder.
	runQCTestSuites False specialisedQFTFullAdderTestSuites
	-- runQCTestSuites True mathTestSuites
	-- runQCTestSuites True registerShiftTestSuites
	-- runAllTests

runAllTests :: IO ()
runAllTests = do
	putStrLn "Running math test suites..."
	runQCTestSuites True mathTestSuites
	putStrLn "Running register shift test suites..."
	runQCTestSuites True registerShiftTestSuites