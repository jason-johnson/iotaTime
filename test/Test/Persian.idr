module Test.Persian

import IotaTime
import IotaTime.Calendar.Persian
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

pymd : CalendarDate Persian -> (Year, PersianMonth, DayOfMonth)
pymd date = case yearMonthDay date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

arithmeticPymd : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  CalendarDate (ArithmeticPersian rule) -> (Year, PersianMonth, DayOfMonth)
arithmeticPymd {rule} date =
  case yearMonthDay date of
    (valueYear ** (valueMonth, valueDay)) =>
      (valueYear, valueMonth, valueDay)

persianRoundTrips : Integer -> Integer -> Bool
persianRoundTrips final current =
  if current > final
    then True
    else case IotaTime.Calendar.Persian.refineDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = pymd date
         in case IotaTime.Calendar.Persian.refineDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays rebuilt == current &&
                persianRoundTrips final (current + 97)

arithmeticPersianRoundTrips : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> Integer -> Bool
arithmeticPersianRoundTrips {rule} final current =
  if current > final
    then True
    else case IotaTime.Calendar.Persian.refineArithmeticDays {rule} current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = arithmeticPymd {rule} date
         in case IotaTime.Calendar.Persian.refineArithmeticRuleDate {rule}
              valueDay valueMonth valueYear of
                Left _ => False
                Right rebuilt =>
                  toDays rebuilt == current &&
                  arithmeticPersianRoundTrips {rule} final (current + 997)

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

nowruz1404 : Either CalendarConversionError (CalendarDate Persian)
nowruz1404 = IotaTime.Calendar.withCalendar (IotaTime.Calendar.Gregorian.calendarDate 21 March 2025)

mixedPersianResult : CalendarDateTime Persian
mixedPersianResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0)
    (IotaTime.Calendar.Persian.calendarDate 31 PersianMonths.Shahrivar 1400))

persianCases : List RuntimeCase
persianCases =
  [ MkRuntimeCase "Persian epoch is 1 Farvardin 1"
      (pymd (IotaTime.Calendar.Persian.fromDays (-503284)) ==
        (1, PersianMonths.Farvardin, 1))
  , MkRuntimeCase "Persian date show uses its public constructor"
      (show (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1) ==
        "calendarDate 1 Farvardin 1")
  , MkRuntimeCase "Persian conversion samples the complete supported range"
      (let first = toDays
             (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1)
           final = toDays
             (IotaTime.Calendar.Persian.calendarDate 29 PersianMonths.Esfand 1500)
        in persianRoundTrips final first &&
       case IotaTime.Calendar.Persian.refineDays final of
         Right date => pymd date == (1500, PersianMonths.Esfand, 29)
         Left _ => False)
  , MkRuntimeCase "Persian epoch matches Julian March 19 622"
      (calendarDays (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1) ==
       calendarDays (IotaTime.Calendar.Julian.calendarDate 19 JulianMonths.March 622))
  , MkRuntimeCase "Nowruz 1400 is Gregorian March 21 2021"
      (calendarDays (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1400) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 21 March 2021))
  , MkRuntimeCase "Nowruz 1399 is Gregorian March 20 2020"
      (calendarDays (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1399) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 20 March 2020))
  , MkRuntimeCase "astronomical Nowruz 1404 is Gregorian March 21 2025"
      (case nowruz1404 of
        Left _ => False
        Right date => pymd date == (1404, PersianMonths.Farvardin, 1))
  , MkRuntimeCase "arithmetic Nowruz 1404 is Gregorian March 20 2025"
      (calendarDays
        (IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1404) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 20 March 2025))
  , MkRuntimeCase "simple and Birashk Persian rules remain distinct types"
      (calendarDays (IotaTime.Calendar.Persian.simpleCalendarDate 1 PersianMonths.Farvardin 1) ==
       calendarDays (IotaTime.Calendar.Julian.calendarDate 18 JulianMonths.March 622) &&
       calendarDays (IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1) ==
       calendarDays (IotaTime.Calendar.Julian.calendarDate 19 JulianMonths.March 622))
  , MkRuntimeCase "arithmetic Persian matches Calendrical Calculations fixtures"
      (calendarDays
        (IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1016) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 21 March 1637) &&
       calendarDays
        (IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1082) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 22 March 1703) &&
       calendarDays
        (IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1796) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 20 March 2417))
  , MkRuntimeCase "arithmetic Persian supports complete years through 9377"
      (case IotaTime.Calendar.Persian.refineArithmeticDate 29 PersianMonths.Esfand 9377 of
        Left _ => False
        Right date => arithmeticPymd {rule = Birashk} date ==
          (9377, PersianMonths.Esfand, 29))
  , MkRuntimeCase "both arithmetic rules round-trip their supported ranges"
      (let simpleFirst = arithmeticNewYearDay {rule = Simple} 1
           simpleFinal = arithmeticNewYearDay {rule = Simple} 9378 - 1
           arithmeticFirst = arithmeticNewYearDay {rule = Birashk} 1
           arithmeticFinal =
             arithmeticNewYearDay {rule = Birashk} 9378 - 1
        in arithmeticPersianRoundTrips {rule = Simple}
             simpleFinal simpleFirst &&
           arithmeticPersianRoundTrips {rule = Birashk}
             arithmeticFinal arithmeticFirst)
  , MkRuntimeCase "arithmetic Persian rejects years above 9377"
      (isLeft (IotaTime.Calendar.Persian.refineSimpleDate
        1 PersianMonths.Farvardin 9378) &&
       isLeft (IotaTime.Calendar.Persian.refineArithmeticDate
        1 PersianMonths.Farvardin 9378))
  , MkRuntimeCase "arithmetic Persian patterns round-trip"
      (case parse (pR {calendar = PersianArithmetic}) "1404-01-01" of
        Left _ => False
        Right date => calendarDays date == calendarDays
          (IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1404))
  , MkRuntimeCase "1 Dey 1348 is Gregorian December 22 1969"
      (calendarDays (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Dey 1348) ==
       calendarDays (IotaTime.Calendar.Gregorian.calendarDate 22 December 1969))
  , MkRuntimeCase "Farvardin has thirty-one days"
      (pymd (IotaTime.Calendar.Persian.calendarDate 31 PersianMonths.Farvardin 1400) ==
        (1400, PersianMonths.Farvardin, 31))
  , MkRuntimeCase "Mehr rejects day thirty-one"
      (isLeft (IotaTime.Calendar.Persian.refineDate 31 PersianMonths.Mehr 1400))
  , MkRuntimeCase "Esfand day thirty exists in leap year 1399"
      (pymd (IotaTime.Calendar.Persian.calendarDate 30 PersianMonths.Esfand 1399) ==
        (1399, PersianMonths.Esfand, 30))
  , MkRuntimeCase "Esfand day thirty exists in leap year 1403"
      (pymd (IotaTime.Calendar.Persian.calendarDate 30 PersianMonths.Esfand 1403) ==
        (1403, PersianMonths.Esfand, 30))
  , MkRuntimeCase "1407 is common in the five-year astronomical gap"
      (isLeft (IotaTime.Calendar.Persian.refineDate 30 PersianMonths.Esfand 1407))
  , MkRuntimeCase "years below one are rejected"
      (isLeft (IotaTime.Calendar.Persian.refineDate 1 PersianMonths.Farvardin 0))
  , MkRuntimeCase "years above 1500 are rejected"
      (isLeft (IotaTime.Calendar.Persian.refineDate 1 PersianMonths.Farvardin 1501))
  , MkRuntimeCase "month periods clamp from Shahrivar into Mehr"
      (pymd (applyPeriod (months 1)
        (IotaTime.Calendar.Persian.calendarDate 31 PersianMonths.Shahrivar 1400)) ==
        (1400, PersianMonths.Mehr, 30))
  , MkRuntimeCase "year periods clamp an Esfand leap day"
      (pymd (applyPeriod (years 1)
        (IotaTime.Calendar.Persian.calendarDate 30 PersianMonths.Esfand 1399)) ==
        (1400, PersianMonths.Esfand, 29))
  , MkRuntimeCase "negative periods clamp at the Persian epoch"
      (pymd (applyPeriod (years (-1))
        (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Mehr 1)) ==
        (1, PersianMonths.Farvardin, 1))
  , MkRuntimeCase "positive periods clamp at the Persian upper bound"
      (pymd (applyPeriod (years 1)
        (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1500)) ==
        (1500, PersianMonths.Esfand, 29))
  , MkRuntimeCase "Persian epoch weekday is Friday"
      (dayOfWeek
        (IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 1) ==
        PersianWeekdays.Friday)
  , MkRuntimeCase "first Monday of Farvardin 1400"
      (dayOfWeek
        (IotaTime.Calendar.Persian.fromNthDay First PersianWeekdays.Monday
          PersianMonths.Farvardin 1400) == PersianWeekdays.Monday)
  , MkRuntimeCase "Persian week one starts on Saturday"
      (dayOfWeek
        (IotaTime.Calendar.Persian.fromWeekDate
          1 PersianWeekdays.Saturday 1400) ==
        PersianWeekdays.Saturday)
  , MkRuntimeCase "Persian CalendarDateTime accepts mixed periods"
      (pymd (datePart mixedPersianResult) ==
        (1400, PersianMonths.Aban, 1) &&
       timeComponents (localTimeOfDay mixedPersianResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Persian calendar tests" persianCases
