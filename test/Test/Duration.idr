module Test.Duration

import IotaTime
import Test.Support

baseInstant : Instant
baseInstant = fromNanosecondsSinceEpoch 100

fiftyNanoseconds : Duration
fiftyNanoseconds = IotaTime.Duration.fromNanoseconds 50

durationCases : List RuntimeCase
durationCases =
  [ MkRuntimeCase "HodaTime duration constructors retain module-qualified names"
      (IotaTime.Duration.fromHours 2 == IotaTime.Duration.fromMinutes 120)
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
      (toDurationNanoseconds (IotaTime.Duration.fromStandardWeeks 1) ==
        7 * 24 * 60 * 60 * 1000000000)
  , MkRuntimeCase "negative duration values are preserved"
      (toDurationNanoseconds (IotaTime.Duration.fromMicroseconds (-17)) == -17000)
  , MkRuntimeCase "duration addition is scalar addition"
      (IotaTime.Duration.add (IotaTime.Duration.fromSeconds 2)
        (IotaTime.Duration.fromMilliseconds 500) ==
        IotaTime.Duration.fromMilliseconds 2500)
  , MkRuntimeCase "duration subtraction can produce a negative result"
      (IotaTime.Duration.minus (IotaTime.Duration.fromSeconds 1)
        (IotaTime.Duration.fromSeconds 3) ==
        IotaTime.Duration.fromSeconds (-2))
  , MkRuntimeCase "adding a duration advances an instant"
      (toNanosecondsSinceEpoch
        (IotaTime.Instant.add baseInstant fiftyNanoseconds) == 150)
  , MkRuntimeCase "subtracting reverses duration addition"
      (IotaTime.Instant.minus
        (IotaTime.Instant.add baseInstant fiftyNanoseconds)
        fiftyNanoseconds == baseInstant)
  , MkRuntimeCase "instant difference is signed"
      (difference baseInstant
        (IotaTime.Instant.add baseInstant fiftyNanoseconds) ==
        IotaTime.Duration.fromNanoseconds (-50))
  , MkRuntimeCase "Unix nanosecond conversion round-trips"
      (toNanosecondsSinceUnixEpoch (fromNanosecondsSinceUnixEpoch (-1)) == -1)
  ]

export
run : IO Bool
run = runSuite "duration and instant tests" durationCases
