module Main

import Test.Proof
import Test.Runtime
import Test.Regression
import Test.Support

proofsChecked : ()
proofsChecked =
  let _ = zeroTickRoundTrip in
  let _ = negativeTickRoundTrip in
  let _ = largeTickRoundTrip in
  let _ = roundTripConstructor in
  ()

main : IO ()
main = do
  let _ = proofsChecked
  runtimePassed <- Test.Runtime.run
  regressionPassed <- Test.Regression.run
  finalizeResults
    [ ("proof tests (compile-time)", True)
    , ("runtime behavior tests", runtimePassed)
    , ("regression tests", regressionPassed)
    ]
