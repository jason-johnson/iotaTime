module Test.PatternDuration

import IotaTime
import Test.Support

parsesAs : Pattern Duration Duration -> String -> Duration -> Bool
parsesAs pattern source expected = case IotaTime.Pattern.parse pattern source of
  Left _ => False
  Right actual => actual == expected

rejects : Pattern Duration Duration -> String -> Bool
rejects pattern source = case IotaTime.Pattern.parse pattern source of
  Left _ => True
  Right _ => False

patternDurationCases : List RuntimeCase
patternDurationCases =
  [ MkRuntimeCase "duration pattern formats day and clock components"
      (IotaTime.Pattern.format pDuration
        (IotaTime.Duration.add (fromStandardDays 1) (fromSeconds 30)) ==
          "1:00:00:30" &&
       IotaTime.Pattern.format pDuration (fromSeconds (-30)) ==
          "-0:00:00:30")
  , MkRuntimeCase "duration pattern parses positive and negative values"
      (parsesAs pDuration "1:02:03:04"
        (fromSeconds (((1 * 24 + 2) * 60 + 3) * 60 + 4)) &&
       parsesAs pDuration "-0:00:00:30" (fromSeconds (-30)))
  , MkRuntimeCase "nanosecond duration pattern preserves precision"
      (IotaTime.Pattern.format pDurationNano (fromNanoseconds (-1)) ==
        "-0:00:00:00.000000001" &&
       parsesAs pDurationNano "-0:00:00:00.000000001"
        (fromNanoseconds (-1)) &&
       parsesAs pDurationNano "2:03:04:05.123456789"
        (fromNanoseconds
          (((((2 * 24 + 3) * 60 + 4) * 60 + 5) * 1000000000) +
            123456789)))
  , MkRuntimeCase "duration pattern supports arbitrarily large day counts"
      (let days = 999999999999999999999999 in
        parsesAs pDuration
          "999999999999999999999999:23:59:59"
          (fromSeconds ((((days * 24) + 23) * 60 + 59) * 60 + 59)))
  , MkRuntimeCase "duration pattern composes with trailing literals"
      (let pattern = pDuration <% string " elapsed" in
        IotaTime.Pattern.format pattern (fromSeconds 90) ==
          "0:00:01:30 elapsed" &&
        parsesAs pattern "0:00:01:30 elapsed" (fromSeconds 90))
  , MkRuntimeCase "duration patterns reject malformed components"
      (rejects pDuration "0:00:60:00" &&
       rejects pDuration "0:00:00:60" &&
       rejects pDuration "+0:00:00:01" &&
       rejects pDurationNano "0:00:00:01.123" &&
       rejects pDurationNano "0:00:00:01.1234567890")
  ]

export
run : IO Bool
run = runSuite "duration pattern tests" patternDurationCases