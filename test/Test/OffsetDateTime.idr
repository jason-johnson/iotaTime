module Test.OffsetDateTime

import IotaTime
import Test.Support

gregorianComponents : OffsetDateTime Gregorian ->
  ((Year, Month, DayOfMonth), (Hour, Minute, Second, Nanosecond), Integer)
gregorianComponents value =
  let local = localDateTime value
      date = datePart local
      time = localTimeOfDay local
   in case yearMonthDay {calendar = Gregorian} date of
        (valueYear ** (valueMonth, valueDay)) =>
          ((valueYear, valueMonth, valueDay),
           (hour time, minute time, second time, nanosecond time),
           totalOffsetSeconds (offsetOf value))

epochAtUtc : OffsetDateTime Gregorian
epochAtUtc = atOffset
  (on (localTime 0 0 0 0) (calendarDate 1 March 2000))
  zeroOffset

epochAtPlusOne : OffsetDateTime Gregorian
epochAtPlusOne = atOffset
  (on (localTime 1 0 0 0) (calendarDate 1 March 2000))
  (offsetFromHours 1)

gregorianBoundaryAtMaxOffset : OffsetDateTime Gregorian
gregorianBoundaryAtMaxOffset = atOffset
  (on (localTime 0 0 0 0) (calendarDate 15 October 1582))
  (offsetFromHours 18)

offsetDateTimeCases : List RuntimeCase
offsetDateTimeCases =
  [ MkRuntimeCase "UTC calendar epoch resolves to the instant epoch"
      (toInstant epochAtUtc == epoch)
  , MkRuntimeCase "local time minus offset resolves to the instant epoch"
      (toInstant epochAtPlusOne == epoch)
  , MkRuntimeCase "instant displays at the requested positive offset"
      (case the (Either CalendarConversionError (OffsetDateTime Gregorian))
        (fromInstant (offsetFromMinutes 90) epoch) of
          Right value => gregorianComponents value ==
            ((2000, March, 1), (1, 30, 0, 0), 5400)
          Left _ => False)
  , MkRuntimeCase "instant conversion handles a negative nanosecond"
      (case the (Either CalendarConversionError (OffsetDateTime Gregorian))
        (fromInstant zeroOffset (fromNanosecondsSinceEpoch (-1))) of
          Right value => gregorianComponents value ==
            ((2000, February, 29), (23, 59, 59, 999999999), 0)
          Left _ => False)
  , MkRuntimeCase "changing offset preserves the instant"
      (case withOffset (offsetFromHours (-2)) epochAtPlusOne of
          Right value => toInstant value == epoch
          Left _ => False)
  , MkRuntimeCase "changing offset shifts local time across midnight"
      (case withOffset (offsetFromHours (-2)) epochAtPlusOne of
          Right value => gregorianComponents value ==
            ((2000, February, 29), (22, 0, 0, 0), -7200)
          Left _ => False)
  , MkRuntimeCase "instant round-trip is exact"
      (case the (Either CalendarConversionError (OffsetDateTime Gregorian))
        (fromInstant (offsetFromSeconds 5445)
          (fromNanosecondsSinceEpoch 12345678901234567890)) of
          Right value => toInstant value ==
            fromNanosecondsSinceEpoch 12345678901234567890
          Left _ => False)
  , MkRuntimeCase "offset change rejects a local day before calendar range"
      (case withOffset (offsetFromHours (-18))
        gregorianBoundaryAtMaxOffset of
          Left (TargetCalendarOutOfRange "Gregorian" _) => True
          _ => False)
  , MkRuntimeCase "calendar conversion preserves instant and offset"
      (case the (Either CalendarConversionError (OffsetDateTime Julian))
        (IotaTime.OffsetDateTime.withCalendar epochAtPlusOne) of
          Right value => toInstant value == epoch &&
            offsetOf value == offsetFromHours 1
          Left _ => False)
  ]

export
run : IO Bool
run = runSuite "offset date-time tests" offsetDateTimeCases
