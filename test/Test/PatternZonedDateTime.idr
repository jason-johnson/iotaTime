module Test.PatternZonedDateTime

import IotaTime
import Test.Support

utcZone : TimeZone
utcZone = fixedDateTimeZone "UTC" zeroOffset

springZone : TimeZone
springZone = dateTimeZone "Test/Spring"
  (transitionInfo zeroOffset False "STD")
  [(0, transitionInfo (offsetFromHours 1) True "DST")]

fallZone : TimeZone
fallZone = dateTimeZone "Test/Fall"
  (transitionInfo (offsetFromHours 1) True "DST")
  [(0, transitionInfo zeroOffset False "STD")]

zoneProvider : String -> IO (Either String TimeZone)
zoneProvider "UTC" = pure (Right utcZone)
zoneProvider "Test/Spring" = pure (Right springZone)
zoneProvider "Test/Fall" = pure (Right fallZone)
zoneProvider name = pure (Left ("unknown zone: " ++ name))

strictResolver : CalendarDateTime Gregorian -> TimeZone ->
  Either ZonedDateTimeError (ZonedDateTime Gregorian)
strictResolver = fromCalendarDateTimeStrictly

lenientResolver : CalendarDateTime Gregorian -> TimeZone ->
  Either ZonedDateTimeError (ZonedDateTime Gregorian)
lenientResolver = fromCalendarDateTimeLeniently

rejectingResolver : CalendarDateTime Gregorian -> TimeZone ->
  Either String (ZonedDateTime Gregorian)
rejectingResolver _ _ = Left "resolution rejected"

formatCases : List RuntimeCase
formatCases =
  [ MkRuntimeCase "standard zoned pattern formats local time and zone ID"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (IotaTime.ZonedDateTime.fromInstant
          (fromSecondsSinceUnixEpoch 0) utcZone) of
          Left _ => False
          Right value => formatZonedDateTime pZonedDateTime value ==
            "1970-01-01T00:00:00 UTC")
  , MkRuntimeCase "custom zoned pattern controls precision and zone rendering"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (IotaTime.ZonedDateTime.fromInstant
          (fromNanosecondsSinceEpoch 1) utcZone) of
          Left _ => False
          Right value =>
            let pattern = zonedDateTimePattern po
                  (\zoned => " [" ++ IotaTime.ZonedDateTime.zoneId zoned ++ "]")
             in formatZonedDateTime pattern value ==
                  "2000-03-01T00:00:00.000000001 [UTC]")
  ]

parseCases : IO (List RuntimeCase)
parseCases = do
  utc <- parseStandardZonedDateTime zoneProvider strictResolver
    "1970-01-01T00:00:00 UTC"
  skipped <- parseStandardZonedDateTime zoneProvider strictResolver
    "2000-03-01T00:30:00 Test/Spring"
  ambiguous <- parseStandardZonedDateTime zoneProvider strictResolver
    "2000-03-01T00:30:00 Test/Fall"
  lenient <- parseStandardZonedDateTime zoneProvider lenientResolver
    "2000-03-01T00:30:00 Test/Fall"
  unknown <- parseStandardZonedDateTime zoneProvider strictResolver
    "2000-03-01T01:00:00 Missing/Zone"
  rejected <- parseStandardZonedDateTime zoneProvider rejectingResolver
    "2000-03-01T01:00:00 UTC"
  malformed <- parseStandardZonedDateTime zoneProvider strictResolver
    "2000-03-01T01:00:00 Test/Fall extra"
  nanos <- parseZonedDateTimeWith po zoneProvider strictResolver
    "2000-03-01T00:00:00.000000001 UTC"
  pure
    [ MkRuntimeCase "standard zoned parser resolves UTC through its provider"
        (case utc of
          Right value =>
            IotaTime.ZonedDateTime.toInstant value ==
              fromSecondsSinceUnixEpoch 0 &&
            IotaTime.ZonedDateTime.zoneId value == "UTC"
          Left _ => False)
    , MkRuntimeCase "standard zoned parser accepts slash-containing zone IDs"
        (case ambiguous of
          Left (ZonedDateTimeResolutionError DateTimeAmbiguous) => True
          _ => False)
    , MkRuntimeCase "strict zoned parsing preserves skipped-time errors"
        (case skipped of
          Left (ZonedDateTimeResolutionError DateTimeDoesNotExist) => True
          _ => False)
    , MkRuntimeCase "lenient zoned parsing chooses the earliest overlap"
        (case lenient of
          Right value => zonedOffset value == offsetFromHours 1
          Left _ => False)
    , MkRuntimeCase "standard zoned parsing preserves provider failures"
        (case unknown of
          Left (ZonedDateTimeProviderError "unknown zone: Missing/Zone") => True
          _ => False)
    , MkRuntimeCase "standard zoned parsing preserves resolver failures"
        (case rejected of
          Left (ZonedDateTimeResolutionError "resolution rejected") => True
          _ => False)
    , MkRuntimeCase "standard zoned parsing rejects whitespace after zone IDs"
        (case malformed of
          Left (ZonedDateTimeParseError _) => True
          _ => False)
    , MkRuntimeCase "custom zoned parsing preserves nanosecond precision"
        (case nanos of
          Right value => IotaTime.ZonedDateTime.toInstant value ==
            fromNanosecondsSinceEpoch 1
          Left _ => False)
    ]

export
run : IO Bool
run = do
  parsed <- parseCases
  runSuite "zoned date-time pattern tests" (formatCases ++ parsed)