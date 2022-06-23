{-|
Module      : OCLGenerator
Description : Contains functions for compiling "Circuit" ASTs to OpenCL kernels and C++ host code suitable for running through the AWS FPGA platform.
-}
module OCLGenerator(
  OCLProblem,
  compileAndBuildCircuit,
  compileCircuitToQProblemFile
) where

import System.IO

import Definitions

import Data.List
import Data.List.Utils (replace)

import System.Directory

import SQCParser
import Debug.Trace

import Control.Monad

data TargetArch = Bolama | AWS | LOCAL_CPU

targetArch :: TargetArch
targetArch = Bolama

isLocalCPU :: Bool
isLocalCPU = case targetArch of
  LOCAL_CPU -> True
  _ -> False

templatesDirectory = case targetArch of
  Bolama -> "./static/templates_bolama/"
  AWS -> "./static/templates_aws/"
  LOCAL_CPU -> "../qwm_cpu/templates/"

configFiles = case targetArch of
  AWS -> ["cmake_build/CMakeLists.txt", "description.json", "details.rst", "Makefile", "qor.json", "README.rst", "utils.mk", "xrt.ini"]
  Bolama -> ["Makefile", "sdaccel.ini", "utils.mk"]
  LOCAL_CPU -> ["CMakeLists.txt", "buildAndRun.sh"]

buildDirectory = case targetArch of
  AWS -> "./static/generated_bolama/"
  Bolama -> "./static/generated_aws/"
  LOCAL_CPU -> "../qwm_cpu/generated/"

-- | Reads an SQC file with the given file name from the circuits directory, returning its contents.
readSQCFile :: String -> IO String
readSQCFile fileName = do
  handle <- openFile ("static/circuits/"++fileName) ReadMode
  hGetContents handle

-- | Represents all the parameters needed to generate OCL files for some circuit from the templates.
data OCLProblem = OCLProblem {
  n :: RegisterSize, -- ^ Number of qubits in the register
  stateSize :: Int, -- ^ The value of N = n^2 provided to the templates for convenience.
  gateCount :: Int, -- ^ Total number of gates to be applied.
  stateR :: [Float], -- ^ Real part of the input state.
  stateI :: [Float], -- ^ Imaginary part of the input state
  problem :: [Int], -- ^ List of ints representing the gates. Each gate is 4 number: (numeric gate code, target qubit ID, first control, second control), where the controls can take on the values of the target ID to denote no control.
  maxControls :: Int -- ^ Number of controls for the gate with the most controls.
} deriving Show

-- | Returns the numeric gate code of a given quantum "Gate".
gateCodeFromGate :: Gate -> Int
gateCodeFromGate H = 0
gateCodeFromGate X = 2
gateCodeFromGate Y = 3
gateCodeFromGate Z = 4
gateCodeFromGate S = 5
gateCodeFromGate (Rm m) = 1000 + m

-- | Returns a list of controls padded with a target "QubitID" if necessary. 
padControls :: Int -> QubitID -> [Control] -> [Control]
padControls maxControls t cs = let
  go :: Int -> QubitID -> [Control] -> [Control] -> [Control]
  go maxControls t cs padded
    | length padded == maxControls = padded
    | null cs = go maxControls t cs (t:padded)
    | otherwise = go maxControls t (tail cs) (head cs:padded)
  in go maxControls t cs []

-- | Converts a given quantum "Circuit" to its "OCLProblem" representation, suitable to be used to process OCL template files.
-- | Takes an Optional maxControls argument, otherwise the maxControls is found from the circuit.
compileCircuitToOCLProblem :: Circuit -> Maybe Int -> OCLProblem
compileCircuitToOCLProblem (Circuit n gas) maxControls = let
  stateSize = 2^n
  gateCount = length gas
  stateR = 1 : [0 | i <- [1..stateSize-1]]
  stateI = [0 | i <- [0..stateSize-1]]
  mC = case maxControls of 
    Just maxControls -> maxControls
    Nothing -> maximum $ map (length . controls) gas
  problem = concatMap (\(GateApplication g t cs _) -> [gateCodeFromGate g, t] ++ padControls mC t cs) gas
  in OCLProblem n stateSize gateCount stateR stateI problem mC

-- | Copies over files outside the src directory from templates to generated.
copyConfigFiles :: [String] -> IO ()
copyConfigFiles [] = return ()
copyConfigFiles (cf:cfs) = do
  createDirectoryIfMissing True buildDirectory
  createDirectoryIfMissing True (buildDirectory++"src")
  case targetArch of
    AWS -> createDirectoryIfMissing True (buildDirectory++"cmake_build")
    _ -> return ()
  copyFile (templatesDirectory++cf) (buildDirectory++cf)
  copyConfigFiles cfs

-- | Process an input template with some given template values as a key-value map, returning the processed template.
processTemplate :: String -> [(String, String)] -> String
processTemplate t [] = t
processTemplate t ((key, value):vs) = processTemplate (replace ("T{{"++key++"}}") value t) vs

-- | Compiles the generated\/src\/host.cpp by parsing its template and writing to generated\/src\/host.cpp.
compileHostTemplate :: OCLProblem -> IO ()
compileHostTemplate (OCLProblem n stateSize gateCount stateR stateI problem maxControls) = let
  preparedProblem = "{" ++ intercalate ", " (map show problem) ++ "}"
  preparedStateR = "{" ++ intercalate ", " (map show stateR) ++ "}"
  preparedStateI = "{" ++ intercalate ", " (map show stateI) ++ "}"
  templateValues = [("problem", preparedProblem), ("state_r", preparedStateR), ("state_i", preparedStateI), ("n", show n), ("G", show gateCount), ("N", show stateSize), ("P", show $ length problem), ("maxControls", show maxControls)]
  in
    do
      template <- openFile (templatesDirectory++"src/host.cpp") ReadMode >>= hGetContents
      writeFile "./static/generated/src/host.cpp" $ processTemplate template templateValues

-- | Compiles the generated\/src\/kernels.cl by parsing its template and writing to generated\/src\/kernels.cl.
compileKernelTemplate :: OCLProblem -> IO ()
compileKernelTemplate (OCLProblem n stateSize gateCount stateR stateI problem maxControls) = let
  templateValues = [("n", show n), ("G", show gateCount), ("N", show stateSize), ("maxControls", show maxControls)] in do
  src <- openFile (templatesDirectory++"src/kernels.cl") ReadMode
  template <- hGetContents src
  writeFile "./static/generated/src/kernels.cl" $ processTemplate template templateValues

compileLocalCTemplate :: OCLProblem -> IO()
compileLocalCTemplate (OCLProblem n stateSize gateCount stateR stateI problem maxControls) = let
  preparedProblem = "{" ++ intercalate ", " (map show problem) ++ "}"
  templateValues = [("problem", preparedProblem), ("n", show n), ("G", show gateCount), ("N", show stateSize), ("maxControls", show maxControls), ("P", show $ length problem)] in do
  src <- openFile (templatesDirectory++"src/main.c") ReadMode
  template <- hGetContents src
  writeFile (buildDirectory++"src/main.c") $ processTemplate template templateValues

-- | Compiles the given circuit into OCL, parsing the templates from static\/templates and writing the generated OCL project to state\/generated.
compileAndBuildCircuit :: Circuit -> IO ()
compileAndBuildCircuit c = do
  removePathForcibly buildDirectory
  let p = compileCircuitToOCLProblem c Nothing
  copyConfigFiles configFiles
  if not isLocalCPU
    then do
      compileHostTemplate p
      compileKernelTemplate p
    else compileLocalCTemplate p

-- | Compiles the given circuit into a QProblem, suitable for passing into an FPGA host template file of the format:
-- | n problem
-- | where n is the number of qubits, and problem is the QProblem of the form:
-- | (gateCode target control*)*
-- | where there are as many controls as passed into the maxControls arguments. This is because the FPGA design
-- | is static and requires a fixed known number of max controls.
compileCircuitToQProblemFile :: Circuit -> Int -> String -> IO ()
compileCircuitToQProblemFile c maxControls destPath = let
  OCLProblem n _ gateCount _ _ problem _ = compileCircuitToOCLProblem c (Just maxControls)
  preparedProblem = unwords (map show (n:problem))
  in do
    writeFile destPath preparedProblem