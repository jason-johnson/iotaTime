module Test.PatternInstant

import IotaTime
import Test.Support

parsesAs : InstantPattern state -> String -> Instant -> Bool
parsesAs pattern source expected = case parseInstant pattern source of
  Left _ => False
  Right actual => actual == expected

formatsAs : InstantPattern state -> Instant -> String -> Bool
formatsAs pattern value expected = case formatInstant pattern value of
  Left _ => False
  Right actual => actual == expected

rejects : InstantPattern state -> String -> Bool
rejects pattern source = case parseInstant pattern source of
  Left _ => True
  Right _ => False

patternInstantCases : List RuntimeCase
patternInstantCases =
  [ MkRuntimeCase "instant pattern formats and parses the Unix epoch"
      (formatsAs pInstant (fromSecondsSinceUnixEpoch 0)
        "1970-01-01T00:00:00Z" &&
       parsesAs pInstant "1970-01-01T00:00:00Z"
        (fromSecondsSinceUnixEpoch 0))
  , MkRuntimeCase "instant nanosecond pattern preserves precision"
      (let value = fromNanosecondsSinceEpoch (-1) in
        formatsAs pInstantNano value
          "2000-02-29T23:59:59.999999999Z" &&
        parsesAs pInstantNano "2000-02-29T23:59:59.999999999Z" value)
  , MkRuntimeCase "instant pattern crosses the library epoch"
      (formatsAs pInstant epoch "2000-03-01T00:00:00Z" &&
       parsesAs pInstant "2000-03-01T00:00:00Z" epoch &&
       parsesAs pInstant "2000-03-01T00:00:01Z"
        (fromNanosecondsSinceEpoch 1000000000))
  , MkRuntimeCase "custom instant patterns treat date-times as UTC"
      (let pattern = instantPattern ps in
        formatsAs pattern epoch "2000-03-01T00:00:00" &&
        parsesAs pattern "2000-03-01T00:00:00" epoch)
  , MkRuntimeCase "instant patterns reject malformed and trailing input"
      (rejects pInstant "2000-02-30T00:00:00Z" &&
       rejects pInstant "2000-03-01T00:00:00" &&
       rejects pInstant "2000-03-01T00:00:00ZZ" &&
       rejects pInstantNano "2000-03-01T00:00:00.123Z")
  , MkRuntimeCase "instant formatting reports the Gregorian range boundary"
      (case formatInstant pInstant
        (fromNanosecondsSinceEpoch (-100000000000000000000)) of
          Left (TargetCalendarOutOfRange "Gregorian" _) => True
          _ => False)
  ]

export
run : IO Bool
run = runSuite "instant pattern tests" patternInstantCases