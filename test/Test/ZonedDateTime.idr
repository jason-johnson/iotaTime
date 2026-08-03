module Test.ZonedDateTime

import IotaTime
import Test.Support

nanosecondsPerHour : Integer
nanosecondsPerHour = 3600 * 1000000000

springZone : DateTimeZone
springZone = dateTimeZone "Test/Spring" zeroOffset
  [(0, offsetFromHours 1)]

fallZone : DateTimeZone
fallZone = dateTimeZone "Test/Fall" (offsetFromHours 1)
  [(0, zeroOffset)]

localAt : Hour -> Minute -> CalendarDateTime Gregorian
localAt valueHour valueMinute = on
  (localTime valueHour valueMinute 0 0)
  (calendarDate 1 March 2000)

components : ZonedDateTime Gregorian ->
  ((Year, Month, DayOfMonth), (Hour, Minute, Second), Integer)
components value =
  let local = zonedLocalDateTime value
      date = datePart local
      time = localTimeOfDay local
   in case yearMonthDay {calendar = Gregorian} date of
        (valueYear ** (valueMonth, valueDay)) =>
          ((valueYear, valueMonth, valueDay),
           (hour time, minute time, second time),
           totalOffsetSeconds (zonedOffset value))

zonedDateTimeCases : List RuntimeCase
zonedDateTimeCases =
  [ MkRuntimeCase "instant uses the zone offset effective at transition"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone epoch) of
          Right value => components value ==
            ((2000, March, 1), (1, 0, 0), 3600)
          Left _ => False)
  , MkRuntimeCase "stored offset always agrees with the zone"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone (fromNanosecondsSinceEpoch (-1))) of
          Right value => zonedOffset value ==
            zoneOffsetAt (zoneOf value) (zonedInstant value)
          Left _ => False)
  , MkRuntimeCase "changing zone preserves instant and shifts local time"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone epoch) of
          Right original =>
            case withZone (fixedDateTimeZone "UTC-2" (offsetFromHours (-2)))
              original of
                Right changed => zonedInstant changed == epoch &&
                  components changed ==
                    ((2000, February, 29), (22, 0, 0), -7200)
                Left _ => False
          Left _ => False)
  , MkRuntimeCase "changing calendar preserves instant zone and offset"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone epoch) of
          Right original =>
            case the (Either CalendarConversionError (ZonedDateTime Julian))
              (IotaTime.ZonedDateTime.withCalendar original) of
                Right changed => zonedInstant changed == epoch &&
                  zoneId (zoneOf changed) == "Test/Spring" &&
                  zonedOffset changed == offsetFromHours 1
                Left _ => False
          Left _ => False)
  , MkRuntimeCase "resolving a skipped local time stays explicit"
      (case resolveLocal springZone (localAt 0 30) of
          ZonedSkipped => True
          _ => False)
  , MkRuntimeCase "resolving an unambiguous local time attaches its zone"
      (case resolveLocal springZone (localAt 1 30) of
          ZonedUnambiguous value => zoneId (zoneOf value) == "Test/Spring" &&
            zonedInstant value ==
              fromNanosecondsSinceEpoch (nanosecondsPerHour `div` 2)
          _ => False)
  , MkRuntimeCase "resolving an overlap retains both zoned instants"
      (case resolveLocal fallZone (localAt 0 30) of
          ZonedAmbiguous earlier later [] =>
            zonedInstant earlier < zonedInstant later &&
            zonedOffset earlier == offsetFromHours 1 &&
            zonedOffset later == zeroOffset
          _ => False)
  , MkRuntimeCase "elapsed hour crosses spring transition on timeline"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone
          (fromNanosecondsSinceEpoch (negate (nanosecondsPerHour `div` 2)))) of
          Right start =>
            case addZonedDuration (durationFromHours 1) start of
              Right finish => components finish ==
                ((2000, March, 1), (1, 30, 0), 3600) &&
                difference (zonedInstant finish) (zonedInstant start) ==
                  durationFromHours 1
              Left _ => False
          Left _ => False)
  , MkRuntimeCase "local hour into spring gap returns skipped mapping"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone
          (fromNanosecondsSinceEpoch (negate (nanosecondsPerHour `div` 2)))) of
          Right start => case applyZonedPeriod (hours 1) start of
            ZonedSkipped => True
            _ => False
          Left _ => False)
  , MkRuntimeCase "subtracting elapsed duration re-evaluates prior offset"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone
          (fromNanosecondsSinceEpoch (nanosecondsPerHour `div` 2))) of
          Right finish =>
            case subtractZonedDuration (durationFromHours 1) finish of
              Right start => components start ==
                ((2000, February, 29), (23, 30, 0), 0)
              Left _ => False
          Left _ => False)
  ]

export
run : IO Bool
run = runSuite "zoned date-time tests" zonedDateTimeCases
