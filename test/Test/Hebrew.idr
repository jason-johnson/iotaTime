module Test.Hebrew

import IotaTime
import Test.Support

hymd : {numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
  CalendarDate (Hebrew numbering) ->
       (Year, HebrewMonthName, DayOfMonth)
hymd date = case yearMonthDay date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, monthName valueMonth, valueDay)

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

hebrewRoundTrips : Integer -> Integer -> Bool
hebrewRoundTrips current final =
  if current > final
    then True
    else case IotaTime.Calendar.Hebrew.refineDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonthName, valueDay) = hymd date
         in case IotaTime.Calendar.Hebrew.refineDate valueDay valueMonthName valueYear of
              Left _ => False
              Right rebuilt =>
                toDays rebuilt == current &&
                hebrewRoundTrips (current + 1) final

civilNisan : CalendarDate HebrewCivil
civilNisan = IotaTime.Calendar.Hebrew.calendarDate 15 5784 HebrewMonths.Nisan

scripturalNisan : CalendarDate HebrewScriptural
scripturalNisan = IotaTime.Calendar.Hebrew.calendarDate' 15 5784 HebrewMonths.Nisan

mixedHebrewResult : CalendarDateTime HebrewCivil
mixedHebrewResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0) (IotaTime.Calendar.Hebrew.calendarDate 29 5786 HebrewMonths.Shevat))

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

hebrewCases : List RuntimeCase
hebrewCases =
  [ MkRuntimeCase "Hebrew epoch is 1 Tishri 1"
      (hymd (IotaTime.Calendar.Hebrew.fromDays (-2103607)) == (1, TishriName, 1))
  , MkRuntimeCase "Hebrew date show retains year-indexed month data"
      (show civilNisan == "calendarDate' 15 5784 Nisan")
  , MkRuntimeCase "shared day zero is 24 Adar I 5760"
      (hymd (IotaTime.Calendar.Hebrew.fromDays 0) == (5760, AdarIName, 24))
  , MkRuntimeCase "Hebrew flat conversion round-trips around the shared epoch"
      (hebrewRoundTrips (-800) 800)
  , MkRuntimeCase "Hebrew conversion remains exact in distant future years"
          (let days = toDays (IotaTime.Calendar.Hebrew.calendarDate 1 100000 HebrewMonths.Tishri) in
        case IotaTime.Calendar.Hebrew.refineDays days of
          Left _ => False
          Right rebuilt => hymd rebuilt == (100000, TishriName, 1))
  , MkRuntimeCase "16 September 2023 is 1 Tishri 5784"
      (toDays (IotaTime.Calendar.Hebrew.calendarDate 1 5784 HebrewMonths.Tishri) ==
        toDays (IotaTime.Calendar.Gregorian.calendarDate 16 September 2023))
  , MkRuntimeCase "23 April 2024 is 15 Nisan 5784"
      (toDays civilNisan ==
        toDays (IotaTime.Calendar.Gregorian.calendarDate 23 April 2024))
  , MkRuntimeCase "5784 is a Hebrew leap year"
      (IotaTime.Calendar.Hebrew.isLeapYear 5784)
  , MkRuntimeCase "5786 is a common Hebrew year"
      (not (IotaTime.Calendar.Hebrew.isLeapYear 5786))
  , MkRuntimeCase "dynamic Adar I is rejected in a common year"
      (isLeft (IotaTime.Calendar.Hebrew.refineDate 1 AdarIName 5786))
  , MkRuntimeCase "complete-year Cheshvan has day 30"
      (hymd (IotaTime.Calendar.Hebrew.calendarDate 30 5783 HebrewMonths.Cheshvan) ==
        (5783, CheshvanName, 30))
  , MkRuntimeCase "regular-year Cheshvan rejects day 30"
      (isLeft (IotaTime.Calendar.Hebrew.refineDate 30 CheshvanName 5786))
  , MkRuntimeCase "regular-year Kislev has day 30"
      (hymd (IotaTime.Calendar.Hebrew.calendarDate 30 5786 HebrewMonths.Kislev) ==
        (5786, KislevName, 30))
  , MkRuntimeCase "deficient-year Kislev rejects day 30"
      (isLeft (IotaTime.Calendar.Hebrew.refineDate 30 KislevName 5781))
  , MkRuntimeCase "leap-year month period enters Adar I"
      (hymd (applyPeriod (months 1) (IotaTime.Calendar.Hebrew.calendarDate 1 5784 HebrewMonths.Shevat)) ==
        (5784, AdarIName, 1))
  , MkRuntimeCase "common-year month period skips Adar I"
      (hymd (applyPeriod (months 1) (IotaTime.Calendar.Hebrew.calendarDate 1 5786 HebrewMonths.Shevat)) ==
        (5786, AdarName, 1))
  , MkRuntimeCase "month period rolls from Elul into Tishri"
      (hymd (applyPeriod (months 1) (IotaTime.Calendar.Hebrew.calendarDate 1 5785 HebrewMonths.Elul)) ==
        (5786, TishriName, 1))
  , MkRuntimeCase "year period maps Adar I to common-year Adar"
      (hymd (applyPeriod (years 1) (IotaTime.Calendar.Hebrew.calendarDate 30 5784 HebrewMonths.AdarI)) ==
        (5785, AdarName, 29))
  , MkRuntimeCase "civil numbering starts at Tishri"
      (IotaTime.Calendar.Hebrew.monthNumber {numbering = Civil}
        (month civilNisan) == 8)
  , MkRuntimeCase "scriptural numbering starts at Nisan"
      (IotaTime.Calendar.Hebrew.monthNumber {numbering = Scriptural}
        (month scripturalNisan) == 1)
  , MkRuntimeCase "numbering variants share the same flat day"
      (toDays civilNisan == toDays scripturalNisan)
  , MkRuntimeCase "1 Tishri 5784 is Saturday"
      (dayOfWeek
        (IotaTime.Calendar.Hebrew.calendarDate 1 5784 HebrewMonths.Tishri) == HebrewWeekdays.Saturday)
  , MkRuntimeCase "third Monday of Tishri 5784 is day 17"
      (hymd (IotaTime.Calendar.Hebrew.fromNthDay Third HebrewWeekdays.Monday 5784 HebrewMonths.Tishri) ==
        (5784, TishriName, 17))
  , MkRuntimeCase "dynamic absent fifth Hebrew weekday is rejected"
      (isLeft (IotaTime.Calendar.Hebrew.refineNthDay Fifth HebrewWeekdays.Monday 5784 TishriName))
  , MkRuntimeCase "Hebrew week one begins on the preceding Sunday"
      (hymd (IotaTime.Calendar.Hebrew.fromWeekDate
        1 HebrewWeekdays.Sunday 5784) ==
        (5783, ElulName, 24))
  , MkRuntimeCase "dynamic Hebrew week date is accepted"
      (case IotaTime.Calendar.Hebrew.refineWeekDate
        1 HebrewWeekdays.Sunday 5784 of
        Left _ => False
        Right date => hymd date == (5783, ElulName, 24))
  , MkRuntimeCase "Hebrew week dates reject days before the epoch"
      (isLeft (IotaTime.Calendar.Hebrew.refineWeekDate
        (-1000000) HebrewWeekdays.Sunday 5784))
  , MkRuntimeCase "Hebrew CalendarDateTime accepts mixed periods"
      (hymd (datePart mixedHebrewResult) == (5786, NisanName, 1) &&
       timeComponents (localTimeOfDay mixedHebrewResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Hebrew calendar tests" hebrewCases