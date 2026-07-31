module Test.Runtime

import IotaTime
import Test.Support

runtimeCases : List RuntimeCase
runtimeCases =
  [ MkRuntimeCase "epoch has zero ticks" (ticks epoch == 0)
  , MkRuntimeCase "negative ticks are preserved" (ticks (MkInstant (-17)) == -17)
  , MkRuntimeCase "large ticks are preserved"
      (ticks (MkInstant 999999999999999999999999999999) == 999999999999999999999999999999)
  , MkRuntimeCase "round-trip through constructor/destructor preserves ticks"
      (let instant = MkInstant 123456789012345678901234567890 in
         ticks (MkInstant (ticks instant)) == ticks instant)
  ]

export
run : IO Bool
run = runSuite "runtime behavior tests" runtimeCases
