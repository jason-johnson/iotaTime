module Test.PatternZonedDateTime

import IotaTime
import Test.Support

strictResolver : CalendarDateTime Gregorian -> TimeZone ->
  Either ZonedDateTimeError (ZonedDateTime Gregorian)
strictResolver = fromCalendarDateTimeStrictly

lenientResolver : CalendarDateTime Gregorian -> TimeZone ->
  Either ZonedDateTimeError (ZonedDateTime Gregorian)
lenientResolver = fromCalendarDateTimeLeniently

rejectingResolver : CalendarDateTime Gregorian -> TimeZone ->
  Either String (ZonedDateTime Gregorian)
rejectingResolver _ _ = Left "resolution rejected"

export
run : IO Bool
run = do
  loadedUtc <- utc
  loadedNewYork <- timeZone "America/New_York"
  case (loadedUtc, loadedNewYork) of
    (Right utcZone, Right newYorkZone) => do
      let provider : String -> IO (Either String TimeZone)
          provider "UTC" = pure (Right utcZone)
          provider "America/New_York" = pure (Right newYorkZone)
          provider name = pure (Left ("unknown zone: " ++ name))
      let pureProvider : String -> Either String (Either String TimeZone)
          pureProvider "UTC" = Right (Right utcZone)
          pureProvider name = Right (Left ("unknown zone: " ++ name))
      let formatted = IotaTime.ZonedDateTime.fromInstant {calendar = Gregorian}
            (fromSecondsSinceUnixEpoch 0) utcZone
      parsedUtc <- parseStandardZonedDateTime provider strictResolver
        "1970-01-01T00:00:00 UTC"
      skipped <- parseStandardZonedDateTime provider strictResolver
        "2024-03-10T02:30:00 America/New_York"
      ambiguous <- parseStandardZonedDateTime provider strictResolver
        "2024-11-03T01:30:00 America/New_York"
      lenient <- parseStandardZonedDateTime provider lenientResolver
        "2024-11-03T01:30:00 America/New_York"
      unknown <- parseStandardZonedDateTime provider strictResolver
        "2024-03-10T01:00:00 Missing/Zone"
      rejected <- parseStandardZonedDateTime provider rejectingResolver
        "1970-01-01T00:00:00 UTC"
      nanos <- parseZonedDateTimeWith po provider strictResolver
        "2000-03-01T00:00:00.000000001 UTC"
      runSuite "zoned date-time pattern tests"
        [ MkRuntimeCase "standard pattern formats a zoned value"
            (case formatted of
              Right value => formatZonedDateTime pZonedDateTime value ==
                "1970-01-01T00:00:00 UTC"
              Left _ => False)
        , MkRuntimeCase "zoned parsing accepts a non-IO provider"
            (case parseStandardZonedDateTime pureProvider strictResolver
              "1970-01-01T00:00:00 UTC" of
                Right (Right value) => IotaTime.ZonedDateTime.toInstant value ==
                  fromSecondsSinceUnixEpoch 0
                _ => False)
        , MkRuntimeCase "standard parser resolves UTC"
            (case parsedUtc of
              Right value => IotaTime.ZonedDateTime.toInstant value ==
                fromSecondsSinceUnixEpoch 0
              Left _ => False)
        , MkRuntimeCase "strict parser preserves skipped-time errors"
            (case skipped of
              Left (ZonedDateTimeResolutionError DateTimeDoesNotExist) => True
              _ => False)
        , MkRuntimeCase "strict parser preserves ambiguous-time errors"
            (case ambiguous of
              Left (ZonedDateTimeResolutionError DateTimeAmbiguous) => True
              _ => False)
        , MkRuntimeCase "lenient parser chooses the earliest overlap"
            (case lenient of
              Right value => inDst value
              Left _ => False)
        , MkRuntimeCase "parser preserves provider failures"
            (case unknown of
              Left (ZonedDateTimeProviderError "unknown zone: Missing/Zone") => True
              _ => False)
        , MkRuntimeCase "parser preserves resolver failures"
            (case rejected of
              Left (ZonedDateTimeResolutionError "resolution rejected") => True
              _ => False)
        , MkRuntimeCase "custom parser preserves nanosecond precision"
            (case nanos of
              Right value => IotaTime.ZonedDateTime.toInstant value ==
                fromNanosecondsSinceEpoch 1
              Left _ => False)
        ]
    _ => runSuite "zoned date-time pattern tests"
      [MkRuntimeCase "required system zones load" False]
