module Test.Islamic

import IotaTime
import IotaTime.Calendar.Islamic
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

iymd : {pattern : IslamicLeapPattern} ->
       KnownIslamicLeapPattern pattern =>
       CalendarDate (Islamic pattern) -> (Year, IslamicMonth, DayOfMonth)
iymd date = case yearMonthDay {calendar = Islamic pattern} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

islamicRoundTrips : Integer -> Integer -> Bool
islamicRoundTrips current final =
  if current > final
    then True
    else case refineIslamicDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = iymd date
         in case refineIslamicDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = Islamic Base16} rebuilt == current &&
                islamicRoundTrips (current + 1) final

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

modernAnchor : Either CalendarConversionError (CalendarDate IslamicBcl)
modernAnchor = IotaTime.Calendar.withCalendar
  (calendarDate 9 August 2021)

mixedIslamicResult : CalendarDateTime IslamicBcl
mixedIslamicResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0)
    (islamicDate 30 IslamicMonths.DhulQadah 1442))

islamicCases : List RuntimeCase
islamicCases =
  [ MkRuntimeCase "Islamic epoch is 1 Muharram 1"
      (iymd (islamicFromDays (-503166)) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "Islamic conversion round-trips two 30-year cycles"
      (islamicRoundTrips (-503166) (-481904))
  , MkRuntimeCase "Islamic epoch matches Julian July 15 622"
      (calendarDays (islamicDate 1 IslamicMonths.Muharram 1) ==
       calendarDays (julianDate 15 JulianMonths.July 622))
  , MkRuntimeCase "1 Muharram 1443 is Gregorian August 9 2021"
      (calendarDays (islamicDate 1 IslamicMonths.Muharram 1443) ==
       calendarDays (calendarDate 9 August 2021))
  , MkRuntimeCase "1 Ramadan 1443 is Gregorian April 2 2022"
      (calendarDays (islamicDate 1 IslamicMonths.Ramadan 1443) ==
       calendarDays (calendarDate 2 April 2022))
  , MkRuntimeCase "generic conversion reaches 1 Muharram 1443"
      (case modernAnchor of
        Left _ => False
        Right date => iymd date == (1443, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "Muharram has thirty days"
      (iymd (islamicDate 30 IslamicMonths.Muharram 1443) ==
        (1443, IslamicMonths.Muharram, 30))
  , MkRuntimeCase "Safar rejects day thirty"
      (isLeft (refineIslamicDate 30 IslamicMonths.Safar 1443))
  , MkRuntimeCase "Base16 cycle-year 16 is leap"
      (iymd (islamicDate 30 IslamicMonths.DhulHijjah 16) ==
        (16, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "Base15 cycle-year 16 is common"
      (isLeft (refineIslamicDate' {pattern = Base15}
        30 IslamicMonths.DhulHijjah 16))
  , MkRuntimeCase "Base15 cycle-year 15 is leap"
      (iymd (islamicDate' {pattern = Base15}
        30 IslamicMonths.DhulHijjah 15) ==
        (15, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "Indian cycle-year 8 is leap"
      (iymd (islamicDate' {pattern = Indian}
        30 IslamicMonths.DhulHijjah 8) ==
        (8, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "Habash al-Hasib cycle-year 30 is leap"
      (iymd (islamicDate' {pattern = HabashAlHasib}
        30 IslamicMonths.DhulHijjah 30) ==
        (30, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "year zero is rejected"
      (isLeft (refineIslamicDate 1 IslamicMonths.Muharram 0))
  , MkRuntimeCase "month periods enter Safar"
      (iymd (applyPeriod (months 1)
        (islamicDate 30 IslamicMonths.Muharram 1443)) ==
        (1443, IslamicMonths.Safar, 29))
  , MkRuntimeCase "year periods clamp the Base16 leap day"
      (iymd (applyPeriod (years 1)
        (islamicDate 30 IslamicMonths.DhulHijjah 1442)) ==
        (1443, IslamicMonths.DhulHijjah, 29))
  , MkRuntimeCase "negative month periods clamp at the Islamic epoch"
      (iymd (applyPeriod (months (-1))
        (islamicDate 1 IslamicMonths.Muharram 1)) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "negative year periods clamp at the Islamic epoch"
      (iymd (applyPeriod (years (-1))
        (islamicDate 1 IslamicMonths.Ramadan 1)) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "Islamic epoch weekday is Thursday"
      (dayOfWeek {calendar = Islamic Base16}
        (islamicFromDays (-503166)) == IslamicWeekdays.Thursday)
  , MkRuntimeCase "first Monday of Muharram 1443"
      (dayOfWeek {calendar = Islamic Base16}
        (islamicFromNthDay First IslamicWeekdays.Monday
          IslamicMonths.Muharram 1443) == IslamicWeekdays.Monday)
  , MkRuntimeCase "Islamic week one starts on Saturday"
      (dayOfWeek {calendar = Islamic Base16}
        (islamicFromWeekDate 1 IslamicWeekdays.Saturday 1443) ==
        IslamicWeekdays.Saturday)
  , MkRuntimeCase "Islamic CalendarDateTime accepts mixed periods"
      (iymd (datePart mixedIslamicResult) ==
        (1443, IslamicMonths.Muharram, 1) &&
       timeComponents (localTimeOfDay mixedIslamicResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Islamic calendar tests" islamicCases
