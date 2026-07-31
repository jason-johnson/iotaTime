module Test.Support

import System

public export
record RuntimeCase where
  constructor MkRuntimeCase
  name : String
  passed : Bool

runCases : Int -> Int -> List RuntimeCase -> IO (Int, Int)
runCases passedCount totalCount [] = pure (passedCount, totalCount)
runCases passedCount totalCount (test :: tests) =
  if test.passed
    then do
      putStrLn ("  [PASS] " ++ test.name)
      runCases (passedCount + 1) (totalCount + 1) tests
    else do
      putStrLn ("  [FAIL] " ++ test.name)
      runCases passedCount (totalCount + 1) tests

export
runSuite : String -> List RuntimeCase -> IO Bool
runSuite suiteName tests = do
  putStrLn ("Running suite: " ++ suiteName)
  (passedCount, totalCount) <- runCases 0 0 tests
  putStrLn ("Suite summary: " ++ show passedCount ++ "/" ++ show totalCount ++ " passed")
  pure (passedCount == totalCount)

allPassed : List (String, Bool) -> Bool
allPassed [] = True
allPassed ((_, passed) :: xs) = passed && allPassed xs

printSuiteSummary : List (String, Bool) -> IO ()
printSuiteSummary [] = pure ()
printSuiteSummary ((suiteName, passed) :: xs) = do
  putStrLn ("  " ++ suiteName ++ ": " ++ if passed then "PASS" else "FAIL")
  printSuiteSummary xs

export
finalizeResults : List (String, Bool) -> IO ()
finalizeResults suiteResults = do
  putStrLn "Final test summary:"
  printSuiteSummary suiteResults
  if allPassed suiteResults
    then putStrLn "All iotaTime tests passed"
    else do
      putStrLn "iotaTime tests failed"
      exitWith (ExitFailure 1)
