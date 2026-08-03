module Test.Duration

import IotaTime
import Test.Support

baseInstant : Instant
baseInstant = fromNanosecondsSinceEpoch 100

fiftyNanoseconds : Duration
fiftyNanoseconds = durationFromNanoseconds 50

durationCases : List RuntimeCase
durationCases =
  [ MkRuntimeCase "HodaTime duration constructors retain module-qualified names"
      (IotaTime.Duration.fromHours 2 == durationFromMinutes 120)
  , MkRuntimeCase "HodaTime duration arithmetic names remain available"
      (IotaTime.Duration.minus
        (IotaTime.Duration.add (IotaTime.Duration.fromSeconds 2)
          (IotaTime.Duration.fromMilliseconds 500))
        (IotaTime.Duration.fromMilliseconds 500) ==
        IotaTime.Duration.fromSeconds 2)
  , MkRuntimeCase "HodaTime instant arithmetic names remain available"
      (IotaTime.Instant.minus
        (IotaTime.Instant.add baseInstant fiftyNanoseconds)
        fiftyNanoseconds == baseInstant)
  , MkRuntimeCase "duration units convert exactly"
      (toDurationNanoseconds (durationFromStandardWeeks 1) ==
        7 * 24 * 60 * 60 * 1000000000)
  , MkRuntimeCase "negative duration values are preserved"
      (toDurationNanoseconds (durationFromMicroseconds (-17)) == -17000)
  , MkRuntimeCase "duration addition is scalar addition"
      (addDurations (durationFromSeconds 2) (durationFromMilliseconds 500) ==
        durationFromMilliseconds 2500)
  , MkRuntimeCase "duration subtraction can produce a negative result"
      (subtractDurations (durationFromSeconds 1) (durationFromSeconds 3) ==
        durationFromSeconds (-2))
  , MkRuntimeCase "duration negation reverses direction"
      (negateDuration (durationFromMinutes 3) == durationFromMinutes (-3))
  , MkRuntimeCase "duration scaling multiplies the fixed amount"
      (scaleDuration 4 (durationFromMilliseconds 250) == durationFromSeconds 1)
  , MkRuntimeCase "adding a duration advances an instant"
      (toNanosecondsSinceEpoch (addDuration baseInstant fiftyNanoseconds) == 150)
  , MkRuntimeCase "subtracting reverses duration addition"
      (subtractDuration (addDuration baseInstant fiftyNanoseconds) fiftyNanoseconds ==
        baseInstant)
  , MkRuntimeCase "instant difference is signed"
      (difference baseInstant (addDuration baseInstant fiftyNanoseconds) ==
        durationFromNanoseconds (-50))
  , MkRuntimeCase "Unix nanosecond conversion round-trips"
      (toNanosecondsSinceUnixEpoch (fromNanosecondsSinceUnixEpoch (-1)) == -1)
  , MkRuntimeCase "large duration arithmetic does not overflow"
      (toDurationNanoseconds
        (scaleDuration 999999999999999999
          (durationFromNanoseconds 999999999999999999)) ==
        999999999999999998000000000000000001)
  ]

export
run : IO Bool
run = runSuite "duration and instant tests" durationCases
