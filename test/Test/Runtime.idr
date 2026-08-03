module Test.Runtime

import IotaTime
import Test.Support

runtimeCases : List RuntimeCase
runtimeCases =
  [ MkRuntimeCase "epoch has zero ticks" (ticks epoch == 0)
  , MkRuntimeCase "negative ticks are preserved"
      (ticks (fromNanosecondsSinceEpoch (-17)) == -17)
  , MkRuntimeCase "large ticks are preserved"
      (ticks (fromNanosecondsSinceEpoch 999999999999999999999999999999) ==
        999999999999999999999999999999)
  , MkRuntimeCase "round-trip through constructor/destructor preserves ticks"
      (let instant = fromNanosecondsSinceEpoch 123456789012345678901234567890 in
         ticks (fromNanosecondsSinceEpoch (ticks instant)) == ticks instant)
  , MkRuntimeCase "Unix epoch precedes the library epoch by 11017 days"
      (ticks (fromSecondsSinceUnixEpoch 0) ==
        (-11017) * 86400 * 1000000000)
  ]

export
run : IO Bool
run = runSuite "runtime behavior tests" runtimeCases
