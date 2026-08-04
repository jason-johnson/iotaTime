module Test.Runtime

import IotaTime
import Test.Support

runtimeCases : List RuntimeCase
runtimeCases =
  [ MkRuntimeCase "epoch has zero ticks" (toNanosecondsSinceEpoch epoch == 0)
  , MkRuntimeCase "negative ticks are preserved"
      (toNanosecondsSinceEpoch (fromNanosecondsSinceEpoch (-17)) == -17)
  , MkRuntimeCase "large ticks are preserved"
      (toNanosecondsSinceEpoch
        (fromNanosecondsSinceEpoch 999999999999999999999999999999) ==
        999999999999999999999999999999)
  , MkRuntimeCase "round-trip through constructor/destructor preserves ticks"
      (let instant = fromNanosecondsSinceEpoch 123456789012345678901234567890 in
         toNanosecondsSinceEpoch
           (fromNanosecondsSinceEpoch (toNanosecondsSinceEpoch instant)) ==
           toNanosecondsSinceEpoch instant)
  , MkRuntimeCase "Unix epoch precedes the library epoch by 11017 days"
      (toNanosecondsSinceEpoch (fromSecondsSinceUnixEpoch 0) ==
        (-11017) * 86400 * 1000000000)
  ]

export
run : IO Bool
run = runSuite "runtime behavior tests" runtimeCases
