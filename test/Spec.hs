import QCTesting
import QCTestSuites
import Helpers

{-|
  Description : Defines the entry point of testing.
-}
main :: IO ()
main = do
	runQCTestSuites True mathTestSuites
	runQCTestSuites True registerShiftTestSuites
	-- runAllTests

runAllTests :: IO ()
runAllTests = do
	putStrLn "Running math test suites..."
	runQCTestSuites True mathTestSuites
	putStrLn "Running register shift test suites..."
	runQCTestSuites True registerShiftTestSuites