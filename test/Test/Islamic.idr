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

ciymd : {pattern : IslamicLeapPattern} ->
        KnownIslamicLeapPattern pattern =>
        CalendarDate (CivilIslamic pattern) ->
        (Year, IslamicMonth, DayOfMonth)
ciymd date = case yearMonthDay {calendar = CivilIslamic pattern} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

islamicRoundTrips : Integer -> Integer -> Bool
islamicRoundTrips current final =
  if current > final
    then True
    else case IotaTime.Calendar.Islamic.refineDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = iymd date
         in case IotaTime.Calendar.Islamic.refineDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = Islamic Base16} rebuilt == current &&
                islamicRoundTrips (current + 1) final

civilIslamicRoundTrips : Integer -> Integer -> Bool
civilIslamicRoundTrips current final =
  if current > final
    then True
    else case IotaTime.Calendar.Islamic.refineCivilDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = ciymd date
         in case IotaTime.Calendar.Islamic.refineCivilDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = CivilIslamic Base16} rebuilt == current &&
                civilIslamicRoundTrips (current + 1) final

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

modernAnchor : Either CalendarConversionError (CalendarDate IslamicBcl)
modernAnchor = IotaTime.Calendar.withCalendar
  (IotaTime.Calendar.Gregorian.calendarDate 9 August 2021)

civilModernAnchor : Either CalendarConversionError
  (CalendarDate CivilIslamicBcl)
civilModernAnchor = IotaTime.Calendar.withCalendar
  (IotaTime.Calendar.Gregorian.calendarDate 10 August 2021)

mixedIslamicResult : CalendarDateTime IslamicBcl
mixedIslamicResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0)
    (IotaTime.Calendar.Islamic.calendarDate 30 IslamicMonths.DhulQadah 1442))

islamicCases : List RuntimeCase
islamicCases =
  [ MkRuntimeCase "Islamic epoch is 1 Muharram 1"
      (iymd (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "Islamic date show retains its indexed constructor"
      (show (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1) ==
        "calendarDate' 1 Muharram 1")
  , MkRuntimeCase "Islamic conversion round-trips two 30-year cycles"
      (islamicRoundTrips (-503166) (-481904))
  , MkRuntimeCase "Islamic epoch matches Julian July 15 622"
      (calendarDays (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1) ==
       calendarDays (IotaTime.Calendar.Julian.calendarDate 15 JulianMonths.July 622))
  , MkRuntimeCase "1 Muharram 1443 is Gregorian August 9 2021"
      (calendarDays (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1443) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 9 August 2021))
  , MkRuntimeCase "1 Ramadan 1443 is Gregorian April 2 2022"
      (calendarDays (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Ramadan 1443) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 2 April 2022))
  , MkRuntimeCase "generic conversion reaches 1 Muharram 1443"
      (case modernAnchor of
        Left _ => False
        Right date => iymd date == (1443, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "Muharram has thirty days"
      (iymd (IotaTime.Calendar.Islamic.calendarDate 30 IslamicMonths.Muharram 1443) ==
        (1443, IslamicMonths.Muharram, 30))
  , MkRuntimeCase "Safar rejects day thirty"
      (isLeft (IotaTime.Calendar.Islamic.refineDate 30 IslamicMonths.Safar 1443))
  , MkRuntimeCase "Base16 cycle-year 16 is leap"
      (iymd (IotaTime.Calendar.Islamic.calendarDate 30 IslamicMonths.DhulHijjah 16) ==
        (16, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "Base15 cycle-year 16 is common"
      (isLeft (IotaTime.Calendar.Islamic.refineDate' {pattern = Base15}
        30 IslamicMonths.DhulHijjah 16))
  , MkRuntimeCase "Base15 cycle-year 15 is leap"
      (iymd (IotaTime.Calendar.Islamic.calendarDate' {pattern = Base15}
        30 IslamicMonths.DhulHijjah 15) ==
        (15, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "Indian cycle-year 8 is leap"
      (iymd (IotaTime.Calendar.Islamic.calendarDate' {pattern = Indian}
        30 IslamicMonths.DhulHijjah 8) ==
        (8, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "Habash al-Hasib cycle-year 30 is leap"
      (iymd (IotaTime.Calendar.Islamic.calendarDate' {pattern = HabashAlHasib}
        30 IslamicMonths.DhulHijjah 30) ==
        (30, IslamicMonths.DhulHijjah, 30))
  , MkRuntimeCase "year zero is rejected"
      (isLeft (IotaTime.Calendar.Islamic.refineDate 1 IslamicMonths.Muharram 0))
  , MkRuntimeCase "month periods enter Safar"
      (iymd (applyPeriod (months 1)
        (IotaTime.Calendar.Islamic.calendarDate 30 IslamicMonths.Muharram 1443)) ==
        (1443, IslamicMonths.Safar, 29))
  , MkRuntimeCase "year periods clamp the Base16 leap day"
      (iymd (applyPeriod (years 1)
        (IotaTime.Calendar.Islamic.calendarDate 30 IslamicMonths.DhulHijjah 1442)) ==
        (1443, IslamicMonths.DhulHijjah, 29))
  , MkRuntimeCase "negative month periods clamp at the Islamic epoch"
      (iymd (applyPeriod (months (-1))
        (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1)) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "negative year periods clamp at the Islamic epoch"
      (iymd (applyPeriod (years (-1))
        (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Ramadan 1)) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "Islamic epoch weekday is Thursday"
      (dayOfWeek {calendar = Islamic Base16}
        (IotaTime.Calendar.Islamic.calendarDate' {pattern = Base16}
          1 IslamicMonths.Muharram 1) == IslamicWeekdays.Thursday)
  , MkRuntimeCase "first Monday of Muharram 1443"
      (dayOfWeek {calendar = Islamic Base16}
        (IotaTime.Calendar.Islamic.fromNthDay First IslamicWeekdays.Monday
          IslamicMonths.Muharram 1443) == IslamicWeekdays.Monday)
  , MkRuntimeCase "Islamic week one starts on Saturday"
      (dayOfWeek {calendar = Islamic Base16}
        (IotaTime.Calendar.Islamic.fromWeekDate
          1 IslamicWeekdays.Saturday 1443) ==
        IslamicWeekdays.Saturday)
  , MkRuntimeCase "Islamic week dates reject days before the epoch"
      (isLeft (IotaTime.Calendar.Islamic.refineWeekDate
        (-1000000) IslamicWeekdays.Saturday 1443))
  , MkRuntimeCase "Islamic CalendarDateTime accepts mixed periods"
      (iymd (datePart mixedIslamicResult) ==
        (1443, IslamicMonths.Muharram, 1) &&
       timeComponents (localTimeOfDay mixedIslamicResult) == (1, 30, 0, 0))
  , MkRuntimeCase "civil Islamic epoch is 1 Muharram 1"
      (ciymd (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1) ==
        (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "civil Islamic epoch matches Julian July 16 622"
      (calendarDays (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1) ==
        calendarDays (IotaTime.Calendar.Julian.calendarDate 16 JulianMonths.July 622))
  , MkRuntimeCase "civil dates are one timeline day after astronomical dates"
      (calendarDays (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1443) ==
        calendarDays (IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1443) + 1)
  , MkRuntimeCase "civil 1 Muharram 1443 is Gregorian August 10 2021"
      (calendarDays (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1443) ==
        calendarDays (IotaTime.Calendar.Gregorian.calendarDate 10 August 2021))
  , MkRuntimeCase "civil Islamic conversion round-trips two 30-year cycles"
      (civilIslamicRoundTrips (-503165) (-481903))
  , MkRuntimeCase "generic conversion reaches civil 1 Muharram 1443"
      (case civilModernAnchor of
        Left _ => False
        Right date => ciymd date ==
          (1443, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "civil epoch weekday is Friday"
      (dayOfWeek {calendar = CivilIslamic Base16}
        (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1) ==
          IslamicWeekdays.Friday)
  , MkRuntimeCase "civil date show names its constructor"
      (show (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1) ==
        "civilCalendarDate' 1 Muharram 1")
  , MkRuntimeCase "civil Base15 retains its leap pattern"
      (ciymd (IotaTime.Calendar.Islamic.civilCalendarDate' {pattern = Base15}
        30 IslamicMonths.DhulHijjah 15) ==
          (15, IslamicMonths.DhulHijjah, 30) &&
      isLeft (IotaTime.Calendar.Islamic.refineCivilDate' {pattern = Base15}
        30 IslamicMonths.DhulHijjah 16))
  , MkRuntimeCase "civil month periods retain the civil epoch"
      (ciymd (applyPeriod (months (-1))
        (IotaTime.Calendar.Islamic.civilCalendarDate 1 IslamicMonths.Muharram 1)) ==
          (1, IslamicMonths.Muharram, 1))
  , MkRuntimeCase "civil nth-weekday construction uses civil weekdays"
      (dayOfWeek {calendar = CivilIslamic Base16}
        (IotaTime.Calendar.Islamic.civilFromNthDay First IslamicWeekdays.Monday
          IslamicMonths.Muharram 1443) == IslamicWeekdays.Monday)
  , MkRuntimeCase "civil week one starts on Saturday"
      (dayOfWeek {calendar = CivilIslamic Base16}
        (civilFromWeekDate 1 IslamicWeekdays.Saturday 1443) ==
          IslamicWeekdays.Saturday)
  , MkRuntimeCase "civil Islamic week dates reject days before the epoch"
      (isLeft (refineCivilWeekDate
        (-1000000) IslamicWeekdays.Saturday 1443))
  , MkRuntimeCase "civil day-count refinement rejects astronomical epoch"
      (case IotaTime.Calendar.Islamic.refineCivilDays (-503166) of
        Left (InvalidIslamicDayCount (-503166)) => True
        _ => False)
  ]

export
run : IO Bool
run = runSuite "Islamic calendar tests" islamicCases
