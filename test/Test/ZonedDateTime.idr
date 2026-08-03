module Test.ZonedDateTime

import IotaTime
import Test.Support

nanosecondsPerHour : Integer
nanosecondsPerHour = 3600 * 1000000000

springZone : DateTimeZone
springZone = dateTimeZone "Test/Spring"
  (transitionInfo zeroOffset False "STD")
  [(0, transitionInfo (offsetFromHours 1) True "DST")]

fallZone : DateTimeZone
fallZone = dateTimeZone "Test/Fall"
  (transitionInfo (offsetFromHours 1) True "DST")
  [(0, transitionInfo zeroOffset False "STD")]

localAt : Hour -> Minute -> CalendarDateTime Gregorian
localAt valueHour valueMinute = on
  (localTime valueHour valueMinute 0 0)
  (calendarDate 1 March 2000)

components : ZonedDateTime Gregorian ->
  ((Year, Month, DayOfMonth), (Hour, Minute, Second), Integer)
components value =
  let local = IotaTime.ZonedDateTime.toCalendarDateTime value
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
        (IotaTime.ZonedDateTime.fromInstant epoch springZone) of
          Right value => components value ==
            ((2000, March, 1), (1, 0, 0), 3600)
          Left _ => False)
  , MkRuntimeCase "stored offset always agrees with the zone"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone (fromNanosecondsSinceEpoch (-1))) of
          Right value => zonedOffset value ==
            zoneOffsetAt (zoneOf value) (zonedInstant value)
          Left _ => False)
  , MkRuntimeCase "zoned metadata follows the active transition"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (IotaTime.ZonedDateTime.fromInstant epoch springZone) of
          Right value => inDst value && zoneAbbreviation value == "DST" &&
            IotaTime.ZonedDateTime.zoneId value == "Test/Spring"
          Left _ => False)
  , MkRuntimeCase "HodaTime component accessors remain available"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (IotaTime.ZonedDateTime.fromInstant epoch springZone) of
          Right value =>
            (IotaTime.ZonedDateTime.year value,
             IotaTime.ZonedDateTime.month value,
             IotaTime.ZonedDateTime.day value,
             IotaTime.ZonedDateTime.hour value,
             IotaTime.ZonedDateTime.minute value,
             IotaTime.ZonedDateTime.second value,
             IotaTime.ZonedDateTime.nanosecond value) ==
            (2000, March, 1, 1, 0, 0, 0)
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
        , MkRuntimeCase "all constructor returns both overlap mappings"
          (case fromCalendarDateTimeAll (localAt 0 30) fallZone of
            [earlier, later] => zonedInstant earlier < zonedInstant later
            _ => False)
        , MkRuntimeCase "strict constructor rejects a skipped local time"
          (case fromCalendarDateTimeStrictly (localAt 0 30) springZone of
            Left DateTimeDoesNotExist => True
            _ => False)
        , MkRuntimeCase "strict constructor rejects an ambiguous local time"
          (case fromCalendarDateTimeStrictly (localAt 0 30) fallZone of
            Left DateTimeAmbiguous => True
            _ => False)
        , MkRuntimeCase "lenient constructor shifts a skipped time by the gap"
          (case fromCalendarDateTimeLeniently (localAt 0 30) springZone of
            Right value => components value ==
            ((2000, March, 1), (1, 30, 0), 3600)
            Left _ => False)
        , MkRuntimeCase "lenient constructor chooses earliest ambiguity"
          (case fromCalendarDateTimeLeniently (localAt 0 30) fallZone of
            Right value => zonedOffset value == offsetFromHours 1
            Left _ => False)
  , MkRuntimeCase "elapsed hour crosses spring transition on timeline"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone
          (fromNanosecondsSinceEpoch (negate (nanosecondsPerHour `div` 2)))) of
          Right start =>
            case IotaTime.ZonedDateTime.add start (durationFromHours 1) of
              Right finish => components finish ==
                ((2000, March, 1), (1, 30, 0), 3600) &&
                difference (zonedInstant finish) (zonedInstant start) ==
                  durationFromHours 1
              Left _ => False
          Left _ => False)
  , MkRuntimeCase "explicit local period workflow exposes spring gap"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone
          (fromNanosecondsSinceEpoch (negate (nanosecondsPerHour `div` 2)))) of
          Right start => case resolveLocal springZone
            (applyPeriod (hours 1)
              (IotaTime.ZonedDateTime.toCalendarDateTime start)) of
            ZonedSkipped => True
            _ => False
          Left _ => False)
  , MkRuntimeCase "subtracting elapsed duration re-evaluates prior offset"
      (case the (Either CalendarConversionError (ZonedDateTime Gregorian))
        (inZone springZone
          (fromNanosecondsSinceEpoch (nanosecondsPerHour `div` 2))) of
          Right finish =>
            case IotaTime.ZonedDateTime.minus finish (durationFromHours 1) of
              Right start => components start ==
                ((2000, February, 29), (23, 30, 0), 0)
              Left _ => False
          Left _ => False)
  ]

export
run : IO Bool
run = runSuite "zoned date-time tests" zonedDateTimeCases
