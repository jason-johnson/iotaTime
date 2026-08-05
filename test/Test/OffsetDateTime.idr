module Test.OffsetDateTime

import IotaTime
import Test.Support

gregorianComponents : OffsetDateTime Gregorian ->
  ((Year, Month, DayOfMonth), (Hour, Minute, Second, Nanosecond), Offset)
gregorianComponents value =
  let local = toCalendarDateTime value
      date = datePart local
      time = localTimeOfDay local
   in case yearMonthDay {calendar = Gregorian} date of
        (valueYear ** (valueMonth, valueDay)) =>
          ((valueYear, valueMonth, valueDay),
           (hour time, minute time, second time, nanosecond time),
           offset value)

offsetDateTimeCases : List RuntimeCase
offsetDateTimeCases =
  [ MkRuntimeCase "calendar date-time and offset construct a value"
      (let value = the (OffsetDateTime Gregorian)
            (fromCalendarDateTimeWithOffset
              (on (localTime 1 0 0 0) (calendarDate 1 March 2000))
              (IotaTime.Offset.fromHours 1)) in
        gregorianComponents value ==
          ((2000, March, 1), (1, 0, 0, 0), IotaTime.Offset.fromHours 1))
  , MkRuntimeCase "instant and positive offset construct local components"
      (case the (Either CalendarConversionError (OffsetDateTime Gregorian))
        (fromInstantWithOffset (fromNanosecondsSinceEpoch 0)
          (IotaTime.Offset.fromMinutes 90)) of
          Right value => gregorianComponents value ==
            ((2000, March, 1), (1, 30, 0, 0),
             IotaTime.Offset.fromMinutes 90)
          Left _ => False)
  , MkRuntimeCase "instant conversion handles a negative nanosecond"
      (case the (Either CalendarConversionError (OffsetDateTime Gregorian))
        (fromInstantWithOffset (fromNanosecondsSinceEpoch (-1)) empty) of
          Right value => gregorianComponents value ==
            ((2000, February, 29), (23, 59, 59, 999999999), empty)
          Left _ => False)
  , MkRuntimeCase "toCalendarDateTime returns the original local value"
      (let local = on (localTime 23 59 58 7)
            (calendarDate 31 December 2024)
           value = the (OffsetDateTime Gregorian)
             (fromCalendarDateTimeWithOffset local
               (IotaTime.Offset.fromHours (-5)))
        in gregorianComponents value ==
          ((2024, December, 31), (23, 59, 58, 7),
           IotaTime.Offset.fromHours (-5)))
  , MkRuntimeCase "offset returns the constructor offset"
      (let expected = IotaTime.Offset.fromSeconds 5445
           value = the (OffsetDateTime Gregorian)
             (fromCalendarDateTimeWithOffset
               (on (localTime 0 0 0 0) (calendarDate 1 January 2025))
               expected)
        in offset value == expected)
  , MkRuntimeCase "toInstant resolves local time using its offset"
      (let value = the (OffsetDateTime Gregorian)
            (fromCalendarDateTimeWithOffset
              (on (localTime 1 30 0 0) (calendarDate 1 March 2000))
              (IotaTime.Offset.fromMinutes 90))
        in toInstant value == epoch)
  , MkRuntimeCase "withOffset preserves the represented instant"
      (let original = the (OffsetDateTime Gregorian)
            (fromCalendarDateTimeWithOffset
              (on (localTime 0 0 0 0) (calendarDate 2 March 2000)) empty)
        in case IotaTime.OffsetDateTime.withOffset
          (IotaTime.Offset.fromHours 2) original of
            Right converted =>
              toInstant converted == toInstant original &&
              gregorianComponents converted ==
                ((2000, March, 2), (2, 0, 0, 0), IotaTime.Offset.fromHours 2)
            Left _ => False)
  , MkRuntimeCase "withCalendar preserves local time offset and instant"
      (let original = the (OffsetDateTime Gregorian)
            (fromCalendarDateTimeWithOffset
              (on (localTime 12 34 56 7) (calendarDate 1 March 2000))
              (IotaTime.Offset.fromHours 1))
        in case the (Either CalendarConversionError (OffsetDateTime Julian))
          (IotaTime.OffsetDateTime.withCalendar original) of
            Right converted =>
              toInstant converted == toInstant original &&
              offset converted == offset original &&
              localTimeOfDay (toCalendarDateTime converted) ==
                localTimeOfDay (toCalendarDateTime original)
            Left _ => False)
  ]

export
run : IO Bool
run = runSuite "offset date-time tests" offsetDateTimeCases
