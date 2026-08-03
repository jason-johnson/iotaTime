module Test.CalendarDateTime

import IotaTime
import Test.Support

ymd : CalendarDate Gregorian -> (Year, Month, DayOfMonth)
ymd date = case yearMonthDay {calendar = Gregorian} date of
  (valueYear ** (valueMonth, valueDay)) => (valueYear, valueMonth, valueDay)

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

dateTimeComponents : CalendarDateTime Gregorian ->
                     ((Year, Month, DayOfMonth), (Hour, Minute, Second, Nanosecond))
dateTimeComponents value = (ymd (datePart value), timeComponents (localTimeOfDay value))

lateDateTime : CalendarDateTime Gregorian
lateDateTime = on (localTime 23 30 0 0) (calendarDate 31 January 2000)

calendarDateTimeCases : List RuntimeCase
calendarDateTimeCases =
  [ MkRuntimeCase "date-first constructor matches on"
      (let date = calendarDate 31 January 2000
           time = localTime 23 30 0 0 in
        dateTimeComponents (at date time) ==
          dateTimeComponents (on time date))
  , MkRuntimeCase "atStartOfDay constructs midnight"
      (dateTimeComponents (atStartOfDay (calendarDate 1 March 2000)) ==
        ((2000, March, 1), (0, 0, 0, 0)))
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
        (on (localTime 0 0 0 0) (calendarDate 1 March 2000))) ==
          ((2000, February, 29), (23, 59, 59, 999999999)))
  , MkRuntimeCase "date fields precede time carry"
      (dateTimeComponents (applyPeriod (years 1 <+> hours 24)
        (on (localTime 0 0 0 0) (calendarDate 28 February 1999))) ==
          ((2000, February, 29), (0, 0, 0, 0)))
  ]

export
run : IO Bool
run = runSuite "calendar date-time tests" calendarDateTimeCases