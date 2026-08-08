module Test.Julian

import IotaTime
import Test.Support

jymd : CalendarDate Julian -> (Year, JulianMonth, DayOfMonth)
jymd date = case yearMonthDay {calendar = Julian} date of
  (valueYear ** (valueMonth, valueDay)) => (valueYear, valueMonth, valueDay)

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

julianRoundTrips : Integer -> Integer -> Bool
julianRoundTrips current final =
  if current > final
    then True
    else case IotaTime.Calendar.Julian.refineDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = jymd date
         in case IotaTime.Calendar.Julian.refineDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = Julian} rebuilt == current &&
                julianRoundTrips (current + 1) final

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

mixedJulianResult : CalendarDateTime Julian
mixedJulianResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0) (IotaTime.Calendar.Julian.calendarDate 31 JulianMonths.January 1900))

julianCases : List RuntimeCase
julianCases =
  [ MkRuntimeCase "Julian epoch decodes to March 1 2000"
      (jymd (IotaTime.Calendar.Julian.fromDays 0) == (2000, JulianMonths.March, 1))
  , MkRuntimeCase "Julian date show uses its public constructor"
      (show (IotaTime.Calendar.Julian.calendarDate 1 JulianMonths.March 2000) ==
        "calendarDate 1 March 2000")
  , MkRuntimeCase "Julian civil conversion round-trips across two leap cycles"
      (julianRoundTrips (-1461) 1461)
  , MkRuntimeCase "Julian introduction is January 1 year -44"
      (jymd (IotaTime.Calendar.Julian.calendarDate 1 JulianMonths.January (-44)) ==
        (-44, JulianMonths.January, 1))
    , MkRuntimeCase "Julian introduction has the documented flat day"
      (toDays {calendar = Julian} (IotaTime.Calendar.Julian.calendarDate 1 JulianMonths.January (-44)) == -746631)
  , MkRuntimeCase "dynamic date before Julian introduction is rejected"
      (isLeft (IotaTime.Calendar.Julian.refineDate 31 JulianMonths.December (-45)))
    , MkRuntimeCase "dynamic flat day before Julian introduction is rejected"
      (isLeft (IotaTime.Calendar.Julian.refineDays (-746632)))
  , MkRuntimeCase "1900 is a Julian leap year"
      (jymd (IotaTime.Calendar.Julian.calendarDate 29 JulianMonths.February 1900) ==
        (1900, JulianMonths.February, 29))
  , MkRuntimeCase "dynamic common-year Julian leap day is rejected"
      (isLeft (IotaTime.Calendar.Julian.refineDate 29 JulianMonths.February 1901))
  , MkRuntimeCase "Julian month period clamps at target month end"
      (jymd (applyPeriod (months 1) (IotaTime.Calendar.Julian.calendarDate 31 JulianMonths.January 1900)) ==
        (1900, JulianMonths.February, 29))
  , MkRuntimeCase "combined Julian months apply as one component"
      (jymd (applyPeriod (months 1 <+> months 1)
        (IotaTime.Calendar.Julian.calendarDate 31 JulianMonths.January 1900)) ==
          (1900, JulianMonths.March, 31))
  , MkRuntimeCase "Julian year period clamps leap day"
      (jymd (applyPeriod (years 1) (IotaTime.Calendar.Julian.calendarDate 29 JulianMonths.February 1900)) ==
        (1901, JulianMonths.February, 28))
  , MkRuntimeCase "Julian epoch begins on Tuesday"
      (dayOfWeek {calendar = Julian} (IotaTime.Calendar.Julian.fromDays 0) == JulianWeekdays.Tuesday)
  , MkRuntimeCase "third Julian Monday of January 2000"
      (jymd (IotaTime.Calendar.Julian.fromNthDay Third JulianWeekdays.Monday JulianMonths.January 2000) ==
        (2000, JulianMonths.January, 18))
  , MkRuntimeCase "dynamic absent fifth Julian weekday is rejected"
      (isLeft (IotaTime.Calendar.Julian.refineNthDay Fifth JulianWeekdays.Monday
        JulianMonths.February 2001))
  , MkRuntimeCase "Julian week one starts on Sunday"
      (jymd (IotaTime.Calendar.Julian.fromWeekDate
        1 JulianWeekdays.Sunday 2000) ==
        (1999, JulianMonths.December, 27))
  , MkRuntimeCase "Julian week dates reject days before the supported range"
      (isLeft (IotaTime.Calendar.Julian.refineWeekDate
        (-1000000) JulianWeekdays.Sunday 2000))
  , MkRuntimeCase "Julian CalendarDateTime accepts mixed periods"
     (jymd (datePart mixedJulianResult) == (1900, JulianMonths.March, 1) &&
      timeComponents (localTimeOfDay mixedJulianResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Julian calendar tests" julianCases