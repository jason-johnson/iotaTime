module Test.CalendarDateTime

import IotaTime
import Test.Support

ymd : CalendarDate Gregorian -> (Year, Month, DayOfMonth)
ymd date = case yearMonthDay date of
  (valueYear ** (valueMonth, valueDay)) => (valueYear, valueMonth, valueDay)

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

dateTimeComponents : CalendarDateTime Gregorian ->
                     ((Year, Month, DayOfMonth), (Hour, Minute, Second, Nanosecond))
dateTimeComponents value = (ymd (datePart value), timeComponents (localTimeOfDay value))

lateDateTime : CalendarDateTime Gregorian
lateDateTime = on (localTime 23 30 0 0) (IotaTime.Calendar.Gregorian.calendarDate 31 January 2000)

dateBetweenApplies : CalendarDate Gregorian -> CalendarDate Gregorian -> Bool
dateBetweenApplies start end =
  applyPeriod (IotaTime.Calendar.between start end) start == end

dateTimeBetweenApplies : CalendarDateTime Gregorian ->
                         CalendarDateTime Gregorian -> Bool
dateTimeBetweenApplies start end = dateTimeComponents
  (applyPeriod (IotaTime.CalendarDateTime.between start end) start) ==
  dateTimeComponents end

dateTimeBetweenReverses : CalendarDateTime Gregorian ->
                          CalendarDateTime Gregorian -> Bool
dateTimeBetweenReverses start end =
  let forward = IotaTime.CalendarDateTime.between start end
      backward = IotaTime.CalendarDateTime.between end start
   in dateTimeComponents
        (applyPeriod backward (applyPeriod forward start)) ==
      dateTimeComponents start

calendarDateTimeCases : List RuntimeCase
calendarDateTimeCases =
  [ MkRuntimeCase "date-first constructor matches on"
      (let date = IotaTime.Calendar.Gregorian.calendarDate 31 January 2000
           time = localTime 23 30 0 0 in
        dateTimeComponents (at date time) ==
          dateTimeComponents (on time date))
  , MkRuntimeCase "atStartOfDay constructs midnight"
      (dateTimeComponents (atStartOfDay (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000)) ==
        ((2000, March, 1), (0, 0, 0, 0)))
  , MkRuntimeCase "calendar date-time equality compares date and time"
      (let value : CalendarDateTime Gregorian
           value = on (localTime 4 30 2 7) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000)
        in value == value &&
          value /= on (localTime 4 30 2 8) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000))
  , MkRuntimeCase "calendar date-time ordering is civil date then time"
      (the (CalendarDateTime Gregorian)
          (on (localTime 23 0 0 0) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000)) <
        the (CalendarDateTime Gregorian)
          (on (localTime 0 0 0 0) (IotaTime.Calendar.Gregorian.calendarDate 2 March 2000)) &&
       the (CalendarDateTime Gregorian)
          (on (localTime 4 30 2 7) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000)) <
        the (CalendarDateTime Gregorian)
          (on (localTime 4 30 2 8) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000)))
  , MkRuntimeCase "calendar date-time show uses public constructors"
      (show (the (CalendarDateTime Gregorian)
        (on (localTime 4 30 2 7) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000))) ==
        "at (calendarDate 1 March 2000) (localTime 4 30 2 7)")
  , MkRuntimeCase "calendar periods apply to CalendarDateTime"
      (dateTimeComponents (applyPeriod (months 2) lateDateTime) ==
        ((2000, March, 31), (23, 30, 0, 0)))
  , MkRuntimeCase "time periods apply to CalendarDateTime and carry into the date"
      (dateTimeComponents (applyPeriod (hours 2) lateDateTime) ==
        ((2000, February, 1), (1, 30, 0, 0)))
  , MkRuntimeCase "mixed periods apply to CalendarDateTime"
      (dateTimeComponents (applyPeriod (months 1 <+> hours 2) lateDateTime) ==
        ((2000, March, 1), (1, 30, 0, 0)))
  , MkRuntimeCase "negative time carry moves CalendarDateTime backward"
      (dateTimeComponents (applyPeriod (nanoseconds (-1))
        (on (localTime 0 0 0 0) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000))) ==
          ((2000, February, 29), (23, 59, 59, 999999999)))
  , MkRuntimeCase "date fields precede time carry"
      (dateTimeComponents (applyPeriod (years 1 <+> hours 24)
        (on (localTime 0 0 0 0) (IotaTime.Calendar.Gregorian.calendarDate 28 February 1999))) ==
          ((2000, February, 29), (0, 0, 0, 0)))
  , MkRuntimeCase "period equality compares every component"
      (let value : Period (CalendarDateTime Gregorian)
           value = years 1 <+> months 2 <+> weeks 3 <+> days 4 <+>
             hours 5 <+> minutes 6 <+> seconds 7 <+> nanoseconds 8
        in value == value && value /= (value <+> nanoseconds 1))
  , MkRuntimeCase "period show exposes every component"
      (let value : Period (CalendarDateTime Gregorian)
           value = years 1 <+> months 2 <+> weeks 3 <+> days 4 <+>
             hours 5 <+> minutes 6 <+> seconds 7 <+> nanoseconds 8
        in show value == "period 1 2 3 4 5 6 7 8")
  , MkRuntimeCase "calendar date between applies back to its endpoint"
      (dateBetweenApplies (IotaTime.Calendar.Gregorian.calendarDate 31 January 2000)
        (IotaTime.Calendar.Gregorian.calendarDate 2 March 2000))
  , MkRuntimeCase "calendar date between takes the largest non-overshooting month count"
      (let difference = IotaTime.Calendar.between
            (IotaTime.Calendar.Gregorian.calendarDate 31 January 2025) (IotaTime.Calendar.Gregorian.calendarDate 30 March 2025)
        in periodYears difference == 0 &&
          periodMonths difference == 1 &&
          periodDays difference == 30)
  , MkRuntimeCase "calendar date between keeps an exact multi-month result"
      (let difference = IotaTime.Calendar.between
            (IotaTime.Calendar.Gregorian.calendarDate 31 January 2025) (IotaTime.Calendar.Gregorian.calendarDate 31 March 2025)
        in periodMonths difference == 2 && periodDays difference == 0)
  , MkRuntimeCase "calendar date between applies in reverse across a clamped month"
      (dateBetweenApplies (IotaTime.Calendar.Gregorian.calendarDate 30 March 2025)
        (IotaTime.Calendar.Gregorian.calendarDate 31 January 2025))
  , MkRuntimeCase "days-only calendar difference preserves the exact day count"
      (let policy = IotaTime.Calendar.MkDateDifferencePolicy DaysOnly ClampToMonth
           difference = betweenWith policy
             (IotaTime.Calendar.Gregorian.calendarDate 31 January 2025) (IotaTime.Calendar.Gregorian.calendarDate 30 March 2025)
        in periodMonths difference == 0 && periodDays difference == 58)
  , MkRuntimeCase "date-time between spans days and subsecond time"
      (dateTimeBetweenApplies
        (on (localTime 23 59 59 999999999) (IotaTime.Calendar.Gregorian.calendarDate 28 February 2000))
        (on (localTime 0 0 0 1) (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000)))
  , MkRuntimeCase "date-time between reverses by swapping endpoints"
      (dateTimeBetweenReverses
        (on (localTime 6 7 8 9) (IotaTime.Calendar.Gregorian.calendarDate 15 October 2024))
        (on (localTime 20 30 40 50) (IotaTime.Calendar.Gregorian.calendarDate 2 March 2025)))
  ]

export
run : IO Bool
run = runSuite "calendar date-time tests" calendarDateTimeCases