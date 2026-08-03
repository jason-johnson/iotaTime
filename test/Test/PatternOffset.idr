module Test.PatternOffset

import IotaTime
import Test.Support

parsesAs : Pattern Offset Offset -> String -> Offset -> Bool
parsesAs pattern source expected = case IotaTime.Pattern.parse pattern source of
  Left _ => False
  Right actual => actual == expected

rejects : Pattern Offset Offset -> String -> Bool
rejects pattern source = case IotaTime.Pattern.parse pattern source of
  Left _ => True
  Right _ => False

sameOffsetDateTime : OffsetDateTime Gregorian ->
                     OffsetDateTime Gregorian -> Bool
sameOffsetDateTime left right =
  calendarDays (datePart (localDateTime left)) ==
    calendarDays (datePart (localDateTime right)) &&
  localTimeOfDay (localDateTime left) == localTimeOfDay (localDateTime right) &&
  offsetOf left == offsetOf right

patternOffsetCases : List RuntimeCase
patternOffsetCases =
  [ MkRuntimeCase "standard offset pattern formats signed minutes"
      (IotaTime.Pattern.format pOffset (fromHours 2) == "+02:00" &&
       IotaTime.Pattern.format pOffset (fromMinutes (-330)) == "-05:30" &&
       IotaTime.Pattern.format pOffset zeroOffset == "+00:00")
  , MkRuntimeCase "standard offset pattern parses signed minutes"
      (parsesAs pOffset "+02:00" (fromHours 2) &&
       parsesAs pOffset "-05:30" (fromMinutes (-330)))
  , MkRuntimeCase "full offset pattern preserves seconds"
      (IotaTime.Pattern.format pOffsetFull (fromSeconds (-19845)) ==
        "-05:30:45" &&
       IotaTime.Pattern.format pOffsetFull (fromSeconds (-1)) ==
        "-00:00:01" &&
       parsesAs pOffsetFull "-05:30:45" (fromSeconds (-19845)))
  , MkRuntimeCase "UTC offset pattern uses Z only for zero"
      (IotaTime.Pattern.format pOffsetZ zeroOffset == "Z" &&
       IotaTime.Pattern.format pOffsetZ (fromHours 2) == "+02:00" &&
       parsesAs pOffsetZ "Z" zeroOffset &&
       parsesAs pOffsetZ "+02:00" (fromHours 2))
  , MkRuntimeCase "compact offset pattern implements strftime percent-z"
      (IotaTime.Pattern.format pOffsetCompact (fromHours 2) == "+0200" &&
       IotaTime.Pattern.format pOffsetCompact (fromMinutes (-330)) ==
        "-0530" &&
       parsesAs pOffsetCompact "+0200" (fromHours 2) &&
       parsesAs pOffsetCompact "-0530" (fromMinutes (-330)))
  , MkRuntimeCase "offset patterns accept the exact supported bounds"
      (parsesAs pOffset "+18:00" (fromHours 18) &&
       parsesAs pOffset "-18:00" (fromHours (-18)))
  , MkRuntimeCase "offset patterns reject invalid components and bounds"
      (rejects pOffset "+18:01" &&
       rejects pOffset "+02:60" &&
       rejects pOffsetFull "+02:00:60" &&
       rejects pOffsetCompact "+0200suffix")
  , MkRuntimeCase "standard offset date-time pattern round-trips"
      (let expected = atOffset
            (on (localTime 9 0 0 0) (calendarDate 23 April 2024))
            (fromMinutes (-330)) in
        IotaTime.Pattern.format pOffsetDateTime expected ==
          "2024-04-23T09:00:00-05:30" &&
        case IotaTime.Pattern.parse pOffsetDateTime
          "2024-04-23T09:00:00-05:30" of
            Left _ => False
            Right actual => sameOffsetDateTime actual expected)
  ]

export
run : IO Bool
run = runSuite "offset pattern tests" patternOffsetCases