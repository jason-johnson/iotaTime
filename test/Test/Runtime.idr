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
run = do
  let fixedInstant = fromNanosecondsSinceEpoch 42
  observed <- getCurrentInstant (fixedClock fixedInstant)
  fake <- newMutableTestClock epoch
  advanceTestClock fake (IotaTime.Duration.fromSeconds 90)
  advanced <- getCurrentInstant fake
  setTestInstant fake fixedInstant
  reset <- getCurrentInstant fake
  runSuite "runtime behavior tests"
    (runtimeCases ++
      [ MkRuntimeCase "fixed clock returns its configured instant"
          (observed == fixedInstant)
      , MkRuntimeCase "fake clock advances by fixed duration"
          (advanced == IotaTime.Instant.add epoch
            (IotaTime.Duration.fromSeconds 90))
      , MkRuntimeCase "fake clock can be set to an exact instant"
          (reset == fixedInstant)
      ])
