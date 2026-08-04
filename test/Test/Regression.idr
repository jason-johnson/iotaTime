module Test.Regression

import IotaTime
import Test.Support

regressionCases : List RuntimeCase
regressionCases =
  [ MkRuntimeCase "regression: epoch remains canonical zero instant"
      (toNanosecondsSinceEpoch epoch == 0)
  , MkRuntimeCase "regression: negative epoch offsets remain representable"
      (let beforeEpoch = fromNanosecondsSinceEpoch (-1) in
      toNanosecondsSinceEpoch beforeEpoch < toNanosecondsSinceEpoch epoch)
  , MkRuntimeCase "regression: integer identity round-trip remains stable"
      (let value = -999999999999999999999999999999 in
      toNanosecondsSinceEpoch (fromNanosecondsSinceEpoch value) == value)
  ]

export
run : IO Bool
run = runSuite "regression tests" regressionCases
