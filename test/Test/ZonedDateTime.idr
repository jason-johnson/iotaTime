module Test.ZonedDateTime

import IotaTime
import Test.Support

zoneCases : TimeZone -> List RuntimeCase
zoneCases timeZoneValue =
  let winter = IotaTime.ZonedDateTime.fromInstant {calendar = Gregorian}
        (fromSecondsSinceUnixEpoch 1704067200) timeZoneValue
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
      ]

export
run : IO Bool
run = do
  loaded <- timeZone "America/New_York"
  case loaded of
    Left _ => runSuite "zoned date-time tests"
      [MkRuntimeCase "America/New_York loads" False]
    Right timeZoneValue => runSuite "zoned date-time tests"
      (zoneCases timeZoneValue)
