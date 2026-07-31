module Test.Regression

import IotaTime
import Test.Support

regressionCases : List RuntimeCase
regressionCases =
  [ MkRuntimeCase "regression: epoch remains canonical zero instant" (ticks epoch == 0)
  , MkRuntimeCase "regression: negative epoch offsets remain representable"
      (let beforeEpoch = MkInstant (-1) in ticks beforeEpoch < ticks epoch)
  , MkRuntimeCase "regression: integer identity round-trip remains stable"
      (let value = -999999999999999999999999999999 in
         ticks (MkInstant value) == value)
  ]

export
run : IO Bool
run = runSuite "regression tests" regressionCases
