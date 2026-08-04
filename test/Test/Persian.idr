module Test.Persian

import IotaTime
import IotaTime.Calendar.Persian
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

pymd : CalendarDate Persian -> (Year, PersianMonth, DayOfMonth)
pymd date = case yearMonthDay {calendar = Persian} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

persianRoundTrips : Integer -> Integer -> Bool
persianRoundTrips final current =
  if current > final
    then True
    else case refinePersianDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = pymd date
         in case refinePersianDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = Persian} rebuilt == current &&
                persianRoundTrips final (current + 97)

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

nowruz1404 : Either CalendarConversionError (CalendarDate Persian)
nowruz1404 = IotaTime.Calendar.withCalendar (calendarDate 21 March 2025)

mixedPersianResult : CalendarDateTime Persian
mixedPersianResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0)
    (persianDate 31 PersianMonths.Shahrivar 1400))

persianCases : List RuntimeCase
persianCases =
  [ MkRuntimeCase "Persian epoch is 1 Farvardin 1"
      (pymd (persianFromDays (-503284)) ==
        (1, PersianMonths.Farvardin, 1))
  , MkRuntimeCase "Persian conversion samples the complete supported range"
      (let first = toDays {calendar = Persian}
             (persianDate 1 PersianMonths.Farvardin 1)
           final = toDays {calendar = Persian}
             (persianDate 29 PersianMonths.Esfand 1500)
        in persianRoundTrips final first &&
       case refinePersianDays final of
         Right date => pymd date == (1500, PersianMonths.Esfand, 29)
         Left _ => False)
  , MkRuntimeCase "Persian epoch matches Julian March 19 622"
      (calendarDays (persianDate 1 PersianMonths.Farvardin 1) ==
       calendarDays (julianDate 19 JulianMonths.March 622))
  , MkRuntimeCase "Nowruz 1400 is Gregorian March 21 2021"
      (calendarDays (persianDate 1 PersianMonths.Farvardin 1400) ==
       calendarDays (calendarDate 21 March 2021))
  , MkRuntimeCase "Nowruz 1399 is Gregorian March 20 2020"
      (calendarDays (persianDate 1 PersianMonths.Farvardin 1399) ==
       calendarDays (calendarDate 20 March 2020))
  , MkRuntimeCase "astronomical Nowruz 1404 is Gregorian March 21 2025"
      (case nowruz1404 of
        Left _ => False
        Right date => pymd date == (1404, PersianMonths.Farvardin, 1))
  , MkRuntimeCase "1 Dey 1348 is Gregorian December 22 1969"
      (calendarDays (persianDate 1 PersianMonths.Dey 1348) ==
       calendarDays (calendarDate 22 December 1969))
  , MkRuntimeCase "Farvardin has thirty-one days"
      (pymd (persianDate 31 PersianMonths.Farvardin 1400) ==
        (1400, PersianMonths.Farvardin, 31))
  , MkRuntimeCase "Mehr rejects day thirty-one"
      (isLeft (refinePersianDate 31 PersianMonths.Mehr 1400))
  , MkRuntimeCase "Esfand day thirty exists in leap year 1399"
      (pymd (persianDate 30 PersianMonths.Esfand 1399) ==
        (1399, PersianMonths.Esfand, 30))
  , MkRuntimeCase "Esfand day thirty exists in leap year 1403"
      (pymd (persianDate 30 PersianMonths.Esfand 1403) ==
        (1403, PersianMonths.Esfand, 30))
  , MkRuntimeCase "1407 is common in the five-year astronomical gap"
      (isLeft (refinePersianDate 30 PersianMonths.Esfand 1407))
  , MkRuntimeCase "years below one are rejected"
      (isLeft (refinePersianDate 1 PersianMonths.Farvardin 0))
  , MkRuntimeCase "years above 1500 are rejected"
      (isLeft (refinePersianDate 1 PersianMonths.Farvardin 1501))
  , MkRuntimeCase "month periods clamp from Shahrivar into Mehr"
      (pymd (applyPeriod (months 1)
        (persianDate 31 PersianMonths.Shahrivar 1400)) ==
        (1400, PersianMonths.Mehr, 30))
  , MkRuntimeCase "year periods clamp an Esfand leap day"
      (pymd (applyPeriod (years 1)
        (persianDate 30 PersianMonths.Esfand 1399)) ==
        (1400, PersianMonths.Esfand, 29))
  , MkRuntimeCase "negative periods clamp at the Persian epoch"
      (pymd (applyPeriod (years (-1))
        (persianDate 1 PersianMonths.Mehr 1)) ==
        (1, PersianMonths.Farvardin, 1))
  , MkRuntimeCase "positive periods clamp at the Persian upper bound"
      (pymd (applyPeriod (years 1)
        (persianDate 1 PersianMonths.Farvardin 1500)) ==
        (1500, PersianMonths.Esfand, 29))
  , MkRuntimeCase "Persian epoch weekday is Friday"
      (dayOfWeek {calendar = Persian}
        (persianDate 1 PersianMonths.Farvardin 1) ==
        PersianWeekdays.Friday)
  , MkRuntimeCase "first Monday of Farvardin 1400"
      (dayOfWeek {calendar = Persian}
        (persianFromNthDay First PersianWeekdays.Monday
          PersianMonths.Farvardin 1400) == PersianWeekdays.Monday)
  , MkRuntimeCase "Persian week one starts on Saturday"
      (dayOfWeek {calendar = Persian}
        (persianFromWeekDate 1 PersianWeekdays.Saturday 1400) ==
        PersianWeekdays.Saturday)
  , MkRuntimeCase "Persian CalendarDateTime accepts mixed periods"
      (pymd (datePart mixedPersianResult) ==
        (1400, PersianMonths.Aban, 1) &&
       timeComponents (localTimeOfDay mixedPersianResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Persian calendar tests" persianCases
