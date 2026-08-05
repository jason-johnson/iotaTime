module Test.ZonedDateTime

import IotaTime
import Test.Support

zoneCases : TimeZone -> TimeZone -> List RuntimeCase
zoneCases utcZone timeZoneValue =
  let winter = IotaTime.ZonedDateTime.fromInstant {calendar = Gregorian}
        (fromSecondsSinceUnixEpoch 1704067200) timeZoneValue
      beforeSpring = IotaTime.ZonedDateTime.fromInstant {calendar = Gregorian}
        (fromSecondsSinceUnixEpoch 1710052200) timeZoneValue
      skipped = on (localTime 2 30 0 0) (calendarDate 10 March 2024)
      ambiguous = on (localTime 1 30 0 0) (calendarDate 3 November 2024)
      skippedGregorian = the (CalendarDateTime Gregorian) skipped
      ambiguousGregorian = the (CalendarDateTime Gregorian) ambiguous
   in [ MkRuntimeCase "instant conversion exposes local components"
          (case winter of
            Right value =>
              IotaTime.ZonedDateTime.year value == 2023 &&
              IotaTime.ZonedDateTime.month value == December &&
              IotaTime.ZonedDateTime.day value == 31 &&
              IotaTime.ZonedDateTime.hour value == 19 &&
              IotaTime.ZonedDateTime.zoneId value == "America/New_York"
            Left _ => False)
      , MkRuntimeCase "instant conversion round-trips"
          (case winter of
            Right value => IotaTime.ZonedDateTime.toInstant value ==
              fromSecondsSinceUnixEpoch 1704067200
            Left _ => False)
      , MkRuntimeCase "zoned equality retains zone identity"
          (case (winter, IotaTime.ZonedDateTime.withZone utcZone =<< winter) of
            (Right original, Right changed) =>
              IotaTime.ZonedDateTime.toInstant original ==
                IotaTime.ZonedDateTime.toInstant changed &&
              original /= changed
            _ => False)
      , MkRuntimeCase "zoned ordering uses zone ID after instant"
          (case (winter, IotaTime.ZonedDateTime.withZone utcZone =<< winter) of
            (Right original, Right changed) => original < changed
            _ => False)
      , MkRuntimeCase "zoned show reconstructs instant and zone"
          (case IotaTime.ZonedDateTime.fromInstant {calendar = Gregorian}
            epoch utcZone of
              Right value => show value ==
                "fromInstant (fromNanosecondsSinceEpoch 0) (<TimeZone UTC>)"
              Left _ => False)
      , MkRuntimeCase "strict construction rejects a skipped local time"
          (case fromCalendarDateTimeStrictly skippedGregorian timeZoneValue of
            Left DateTimeDoesNotExist => True
            _ => False)
      , MkRuntimeCase "strict construction rejects an ambiguous local time"
          (case fromCalendarDateTimeStrictly ambiguousGregorian timeZoneValue of
            Left DateTimeAmbiguous => True
            _ => False)
      , MkRuntimeCase "all construction retains both overlap mappings"
          (case fromCalendarDateTimeAll ambiguousGregorian timeZoneValue of
            [earlier, later] => IotaTime.ZonedDateTime.toInstant earlier <
              IotaTime.ZonedDateTime.toInstant later
            _ => False)
      , MkRuntimeCase "lenient construction shifts a skipped local time"
          (case fromCalendarDateTimeLeniently skippedGregorian timeZoneValue of
            Right value => IotaTime.ZonedDateTime.hour value == 3 &&
              IotaTime.ZonedDateTime.minute value == 30
            Left _ => False)
      , MkRuntimeCase "calendar conversion preserves instant and zone"
          (case winter of
            Right value => case the
              (Either CalendarConversionError (ZonedDateTime Julian))
              (IotaTime.ZonedDateTime.withCalendar value) of
                Right changed => IotaTime.ZonedDateTime.toInstant changed ==
                  IotaTime.ZonedDateTime.toInstant value &&
                  IotaTime.ZonedDateTime.zoneId changed == "America/New_York"
                Left _ => False
            Left _ => False)
      , MkRuntimeCase "zone conversion preserves instant and recomputes local time"
          (case winter of
            Right value => case IotaTime.ZonedDateTime.withZone utcZone value of
              Right changed => IotaTime.ZonedDateTime.toInstant changed ==
                IotaTime.ZonedDateTime.toInstant value &&
                IotaTime.ZonedDateTime.zoneId changed == "UTC" &&
                IotaTime.ZonedDateTime.hour changed == 0
              Left _ => False
            Left _ => False)
      , MkRuntimeCase "fixed duration addition crosses the spring gap on the timeline"
          (case beforeSpring of
            Right value => case IotaTime.ZonedDateTime.add value
              (IotaTime.Duration.fromHours 1) of
                Right changed => IotaTime.ZonedDateTime.toInstant changed ==
                  IotaTime.Instant.add (IotaTime.ZonedDateTime.toInstant value)
                    (IotaTime.Duration.fromHours 1) &&
                  IotaTime.ZonedDateTime.hour changed == 3 &&
                  IotaTime.ZonedDateTime.minute changed == 30 &&
                  IotaTime.ZonedDateTime.inDst changed
                Left _ => False
            Left _ => False)
      , MkRuntimeCase "fixed duration subtraction reverses addition"
          (case beforeSpring of
            Right value => case IotaTime.ZonedDateTime.add value
              (IotaTime.Duration.fromHours 1) of
                Right changed => case IotaTime.ZonedDateTime.minus changed
                  (IotaTime.Duration.fromHours 1) of
                    Right restored => IotaTime.ZonedDateTime.toInstant restored ==
                      IotaTime.ZonedDateTime.toInstant value
                    Left _ => False
                Left _ => False
            Left _ => False)
      ]

zonedClockCases : TimeZone -> IO (List RuntimeCase)
zonedClockCases utcZone = do
  fake <- newMutableTestClock epoch
  let clock = zonedClock {calendar = Gregorian} fake utcZone
  initial <- getCurrentZonedDateTime clock
  advanceTestClock fake (IotaTime.Duration.fromMinutes 90)
  advanced <- getCurrentZonedDateTime clock
  pure
    [ MkRuntimeCase "zoned clock displays its current instant"
        (case initial of
          Right value => IotaTime.ZonedDateTime.toInstant value == epoch &&
            IotaTime.ZonedDateTime.zoneId value == "UTC"
          Left _ => False)
    , MkRuntimeCase "zoned clock reflects fake clock advancement"
        (case advanced of
          Right value => IotaTime.ZonedDateTime.hour value == 1 &&
            IotaTime.ZonedDateTime.minute value == 30
          Left _ => False)
    ]

export
run : IO Bool
run = do
  loaded <- timeZone "America/New_York"
  loadedUtc <- utc
  case (loadedUtc, loaded) of
    (Right utcZone, Right timeZoneValue) => do
      clockCases <- zonedClockCases utcZone
      runSuite "zoned date-time tests"
        (zoneCases utcZone timeZoneValue ++ clockCases)
    _ => runSuite "zoned date-time tests"
      [MkRuntimeCase "UTC and America/New_York load" False]
