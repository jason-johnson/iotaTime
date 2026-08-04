module Test.Hebrew

import IotaTime
import Test.Support

hymd : {numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
  CalendarDate (Hebrew numbering) ->
       (Year, HebrewMonthName, DayOfMonth)
hymd date = case yearMonthDay {calendar = Hebrew numbering} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, monthName valueMonth, valueDay)

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

hebrewRoundTrips : Integer -> Integer -> Bool
hebrewRoundTrips current final =
  if current > final
    then True
    else case refineHebrewDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonthName, valueDay) = hymd date
         in case refineHebrewDate valueDay valueMonthName valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = HebrewCivil} rebuilt == current &&
                hebrewRoundTrips (current + 1) final

civilNisan : CalendarDate HebrewCivil
civilNisan = hebrewDate 15 5784 HebrewMonths.Nisan

scripturalNisan : CalendarDate HebrewScriptural
scripturalNisan = hebrewDate' 15 5784 HebrewMonths.Nisan

mixedHebrewResult : CalendarDateTime HebrewCivil
mixedHebrewResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0) (hebrewDate 29 5786 HebrewMonths.Shevat))

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

hebrewCases : List RuntimeCase
hebrewCases =
  [ MkRuntimeCase "Hebrew epoch is 1 Tishri 1"
      (hymd (hebrewFromDays (-2103607)) == (1, TishriName, 1))
  , MkRuntimeCase "shared day zero is 24 Adar I 5760"
      (hymd (hebrewFromDays 0) == (5760, AdarIName, 24))
  , MkRuntimeCase "Hebrew flat conversion round-trips around the shared epoch"
      (hebrewRoundTrips (-800) 800)
  , MkRuntimeCase "Hebrew conversion remains exact in distant future years"
          (let days = toDays {calendar = HebrewCivil} (hebrewDate 1 100000 HebrewMonths.Tishri) in
        case refineHebrewDays days of
          Left _ => False
          Right rebuilt => hymd rebuilt == (100000, TishriName, 1))
  , MkRuntimeCase "16 September 2023 is 1 Tishri 5784"
      (toDays {calendar = HebrewCivil} (hebrewDate 1 5784 HebrewMonths.Tishri) ==
        toDays {calendar = Gregorian} (calendarDate 16 September 2023))
  , MkRuntimeCase "23 April 2024 is 15 Nisan 5784"
      (toDays {calendar = HebrewCivil} civilNisan ==
        toDays {calendar = Gregorian} (calendarDate 23 April 2024))
  , MkRuntimeCase "5784 is a Hebrew leap year"
      (isHebrewLeapYear 5784)
  , MkRuntimeCase "5786 is a common Hebrew year"
      (not (isHebrewLeapYear 5786))
  , MkRuntimeCase "dynamic Adar I is rejected in a common year"
      (isLeft (refineHebrewDate 1 AdarIName 5786))
  , MkRuntimeCase "complete-year Cheshvan has day 30"
      (hymd (hebrewDate 30 5783 HebrewMonths.Cheshvan) ==
        (5783, CheshvanName, 30))
  , MkRuntimeCase "regular-year Cheshvan rejects day 30"
      (isLeft (refineHebrewDate 30 CheshvanName 5786))
  , MkRuntimeCase "regular-year Kislev has day 30"
      (hymd (hebrewDate 30 5786 HebrewMonths.Kislev) ==
        (5786, KislevName, 30))
  , MkRuntimeCase "deficient-year Kislev rejects day 30"
      (isLeft (refineHebrewDate 30 KislevName 5781))
  , MkRuntimeCase "leap-year month period enters Adar I"
      (hymd (applyPeriod (months 1) (hebrewDate 1 5784 HebrewMonths.Shevat)) ==
        (5784, AdarIName, 1))
  , MkRuntimeCase "common-year month period skips Adar I"
      (hymd (applyPeriod (months 1) (hebrewDate 1 5786 HebrewMonths.Shevat)) ==
        (5786, AdarName, 1))
  , MkRuntimeCase "month period rolls from Elul into Tishri"
      (hymd (applyPeriod (months 1) (hebrewDate 1 5785 HebrewMonths.Elul)) ==
        (5786, TishriName, 1))
  , MkRuntimeCase "year period maps Adar I to common-year Adar"
      (hymd (applyPeriod (years 1) (hebrewDate 30 5784 HebrewMonths.AdarI)) ==
        (5785, AdarName, 29))
  , MkRuntimeCase "civil numbering starts at Tishri"
      (hebrewMonthNumber {numbering = Civil}
        (month {calendar = HebrewCivil} civilNisan) == 8)
  , MkRuntimeCase "scriptural numbering starts at Nisan"
      (hebrewMonthNumber {numbering = Scriptural}
        (month {calendar = HebrewScriptural} scripturalNisan) == 1)
  , MkRuntimeCase "numbering variants share the same flat day"
      (toDays {calendar = HebrewCivil} civilNisan ==
        toDays {calendar = HebrewScriptural} scripturalNisan)
  , MkRuntimeCase "1 Tishri 5784 is Saturday"
      (dayOfWeek {calendar = HebrewCivil}
        (hebrewDate 1 5784 HebrewMonths.Tishri) == HebrewWeekdays.Saturday)
  , MkRuntimeCase "third Monday of Tishri 5784 is day 17"
      (hymd (hebrewFromNthDay Third HebrewWeekdays.Monday 5784 HebrewMonths.Tishri) ==
        (5784, TishriName, 17))
  , MkRuntimeCase "dynamic absent fifth Hebrew weekday is rejected"
      (isLeft (refineHebrewNthDay Fifth HebrewWeekdays.Monday 5784 TishriName))
  , MkRuntimeCase "Hebrew week one begins on the preceding Sunday"
      (hymd (hebrewFromWeekDate 1 HebrewWeekdays.Sunday 5784) ==
        (5783, ElulName, 24))
  , MkRuntimeCase "dynamic Hebrew week date is accepted"
      (case refineHebrewWeekDate 1 HebrewWeekdays.Sunday 5784 of
        Left _ => False
        Right date => hymd date == (5783, ElulName, 24))
  , MkRuntimeCase "Hebrew CalendarDateTime accepts mixed periods"
      (hymd (datePart mixedHebrewResult) == (5786, NisanName, 1) &&
       timeComponents (localTimeOfDay mixedHebrewResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Hebrew calendar tests" hebrewCases